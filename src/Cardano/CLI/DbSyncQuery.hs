{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.CLI.DbSyncQuery where

import           Cardano.CLI.Fetching (RegistrationInfo (..))
import           Cardano.CLI.Voting.Metadata (MetadataParsingError, Vote, metadataMetaKey,
                   signatureMetaKey, voteFromTxMetadata, voteRegistrationSlot,
                   voteRegistrationStakeAddress, voteRegistrationStakeHash)
import           Cardano.Db
import           Control.Lens ((#))
import           Control.Lens.TH (makeClassyPrisms)
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (ReaderT)
import           Data.ByteString (ByteString)
import           Data.Foldable (foldl')
import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Sum (..))
import           Data.Text (Text)
import           Database.Esqueleto.Legacy (Entity (..), InnerJoin (..), LeftOuterJoin (..),
                   Value (..), from, isNothing, just, on, select, unValue, val, valkey, where_,
                   (&&.), (<=.), (==.), (?.), (^.))
import           Database.Persist.Sql (SqlBackend, toSqlKey)

import qualified Cardano.Api as Api
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified Database.Esqueleto.Experimental as E

data MetadataRetrievalError
  = MetadataFailedToDecodeMetadataField !TxId !Text
  | MetadataFailedToDecodeSignatureField !TxId !Text
  | MetadataFailedToDecodeTxMetadata !TxId !Api.TxMetadataJsonError
  | MetadataFailedToParseVoteRegistration !TxId !MetadataParsingError
  deriving (Eq, Show)

makeClassyPrisms ''MetadataRetrievalError

queryVoteRegistrations
  :: MonadIO m
  => Maybe Api.SlotNo
  -> ReaderT SqlBackend m [( Key Tx
                           -- ^ Tx Id
                           , Aeson.Value
                           -- ^ Registration Tx Metadata
                           )]
queryVoteRegistrations mSlotNo = do
  (metadataRaw :: [(Value (Key Tx), Entity TxMetadata, Entity TxMetadata)]) <-
    select $ from $
    \(metaTable `InnerJoin` tx `InnerJoin` sigTable `InnerJoin` block) -> do
      on (metaTable ^. TxMetadataTxId ==. tx ^. TxId)
      on (sigTable ^. TxMetadataTxId ==. tx ^. TxId)
      on (block ^. BlockId ==. tx ^. TxBlockId)
      where_ (metaTable ^. TxMetadataKey ==. val (DbWord64 $ metadataMetaKey))
      where_ (sigTable ^. TxMetadataKey ==. val (DbWord64 $ signatureMetaKey))
      where_ (case mSlotNo of
                Nothing ->
                  val True
                Just slotNo ->
                  block ^. BlockSlotNo <=. val (Just (Api.unSlotNo slotNo))
             )

      pure (tx ^. TxId, metaTable, sigTable)

  -- Reconstruct full JSON from individual metadata entries
  pure $ flip foldMap metadataRaw $ \( Value txId
                                     , (Entity _idRego regoMeta)
                                     , (Entity _idSig sigMeta)
                                     ) -> do
    let
      regoJSON :: Aeson.Value
      regoJSON = Aeson.Object $ either mempty id $ decodeMetadata regoMeta

      sigJSON :: Aeson.Value
      sigJSON = Aeson.Object $ either mempty id $ decodeMetadata sigMeta

      metaObj :: Aeson.Value
      metaObj = Aeson.Object $ HM.fromList [ ( T.pack $ show metadataMetaKey
                                             , regoJSON
                                             )
                                           , ( T.pack $ show signatureMetaKey
                                             , sigJSON
                                             )
                                           ]

    [(txId, metaObj)]

queryStakeValues
  :: MonadIO m
  => Maybe Api.SlotNo
  -> [Api.StakeAddress]
  -> ReaderT SqlBackend m [(Api.StakeAddress, Integer)]
queryStakeValues mSlotNo stakeAddrs = do
  (stakeValuesRaw :: [(Value ByteString, Value DbLovelace)]) <- E.select $ do
    (txOut E.:& stakeAddress E.:& txIn) <- E.from $
      -- Given a snapshot of the txins and outs before the given slot number,
      txOutSnapshot `E.leftJoin` txInSnapshot
      -- match TxIns & TxOuts on appropriate indices,
      `E.on` (\((txOut E.:& _) E.:& txIn) ->
                E.just (txOut ^. TxOutTxId) E.==. txIn ?. TxInTxOutId
                E.&&. E.just (txOut ^. TxOutIndex) E.==. txIn ?. TxInTxOutIndex)
    -- then return those that haven't been spent.
    E.where_ (isNothing $ txIn ?. TxInTxInId)
    pure ( stakeAddress ^. StakeAddressHashRaw
         , txOut ^. TxOutValue
         )

  let
    -- Ensure every stake hash asked for is represented to ensure that the size
    -- of the input list matches the size of the output list.
    stakeValuesZero :: Map Api.StakeAddress (Sum Integer)
    stakeValuesZero = M.fromList $ (, Sum 0) <$> stakeAddrs

    -- Add a queried stake value to the map. Ensure we sum in an unbounded type
    -- (Integer) to prevent Int overflow. Stake hashes that fail to deserialise
    -- are ignored.
    addStakeValue
      :: Map Api.StakeAddress (Sum Integer)
      -> (Value ByteString, Value DbLovelace)
      -> Map Api.StakeAddress (Sum Integer)
    addStakeValue acc (Value stakeAddrHashRaw, Value (DbLovelace v)) =
      case Api.deserialiseFromRawBytes Api.AsStakeAddress stakeAddrHashRaw of
        Nothing        -> acc
        Just stakeAddr -> M.insertWith (<>) stakeAddr (Sum . fromIntegral $ v) acc

    -- Add all queried stake values.
    stakeValues :: Map Api.StakeAddress (Sum Integer)
    stakeValues = foldl' addStakeValue stakeValuesZero stakeValuesRaw

  -- Return summed stake values as list.
  pure $ M.toList $ fmap getSum stakeValues

  where
    -- A snapshot of all TxIn's that occur before the given slot number.
    txInSnapshot :: E.SqlQuery (E.SqlExpr (Entity TxIn))
    txInSnapshot = do
        (txIn E.:& _tx E.:& block) <-
          E.from $ E.table @TxIn
          `E.innerJoin` E.table @Tx
          `E.on` (\(txIn E.:& tx) ->
                    txIn ^. TxInTxInId E.==. tx ^. TxId)
          `E.innerJoin` E.table @Block
          `E.on` (\(_ E.:& tx E.:& block) ->
                    tx ^. TxBlockId E.==. block ^. BlockId)
        E.where_ (case mSlotNo of
                    Nothing ->
                      val True
                    Just slotNo ->
                      block ^. BlockSlotNo E.<=. E.val (Just (Api.unSlotNo slotNo))
                 )
        pure txIn

    -- A snapshot of all TxOuts that occur before the given slot number and that
    -- are associated with a stake address in the given stake addresses.
    txOutSnapshot :: E.SqlQuery (    E.SqlExpr (Entity TxOut)
                                E.:& E.SqlExpr (Entity StakeAddress)
                                )
    txOutSnapshot = do
        (txOut E.:& stakeAddress E.:& _tx E.:& block) <-
          E.from $ E.table @TxOut
          `E.innerJoin` E.table @StakeAddress
          `E.on` (\(txOut E.:& stakeAddress) ->
                    E.just (stakeAddress ^. StakeAddressId) E.==. txOut ^. TxOutStakeAddressId)
          `E.innerJoin` E.table @Tx
          `E.on` (\(txOut E.:& _ E.:& tx) ->
                    txOut ^. TxOutTxId E.==. tx ^. TxId)
          `E.innerJoin` E.table @Block
          `E.on` (\(_ E.:& _ E.:& tx E.:& block) ->
                    tx ^. TxBlockId E.==. block ^. BlockId)
        E.where_ $ stakeAddress ^. StakeAddressHashRaw
          `E.in_` E.valList (Api.serialiseToRawBytes <$> stakeAddrs)
        E.where_ (case mSlotNo of
                    Nothing ->
                      val True
                    Just slotNo ->
                      block ^. BlockSlotNo E.<=. E.val (Just (Api.unSlotNo slotNo))
                 )
        pure (txOut E.:& stakeAddress)

queryStakeValue :: MonadIO m => Api.Hash Api.StakeKey -> ReaderT SqlBackend m Integer
queryStakeValue stakeHash = do
  values <- select $ from $
    \(txOut `LeftOuterJoin` txIn `InnerJoin` stakeAddress) -> do
      -- Find TxIns for each TxOut.
      on ( (txOut ^. TxOutTxId ==. txIn ^. TxInTxOutId)
           &&. (txOut ^. TxOutIndex ==. txIn ^. TxInTxOutIndex))
      -- Select TxOuts that delegate their value to a stake address.
      on (just (stakeAddress ^. StakeAddressId) ==. txOut ^. TxOutStakeAddressId)
      -- Select TxIns that have not been spent (null TxIn).
      where_ (txIn ^. TxInTxInId ==. valkey 0)
      -- Select only where stake address is matching
      where_ (stakeAddress ^. StakeAddressHashRaw ==. val (Api.serialiseToRawBytes stakeHash))

      pure ( txOut ^. TxOutValue )

  pure $ getSum $ foldMap (Sum . fromIntegral . unDbLovelace . unValue) values

-- ** TODO APPLICATION LAYER
queryVotingFunds
  :: ( MonadIO m )
  => Api.NetworkId
  -> Maybe Api.SlotNo
  -> ReaderT SqlBackend m [RegistrationInfo]
queryVotingFunds nw mSlotNo = do
  regosRaw <- queryVoteRegistrations mSlotNo

  let
    regos =
      flip foldMap regosRaw $ \(txId, regoRaw) -> do
        case parseRegistration regoRaw of
          Left _e -> []
          Right rego -> [(txId, rego)]

  let latestRegos = filterLatestRegistrations regos

  regoStakes <- queryStakeValues mSlotNo $ fmap (voteRegistrationStakeAddress nw) latestRegos
  let regoValues = (zip latestRegos . fmap snd) regoStakes

  pure $ fmap (\(vote, value) -> RegistrationInfo vote value) regoValues

-- Common helpers

filterLatestRegistrations :: Ord a => [(a, Vote)] -> [Vote]
filterLatestRegistrations regos =
  fmap snd $ M.elems $ foldl' (flip accumulateRegistrations) mempty regos


accumulateRegistrations
  :: Ord a
  => (a, Vote)
  -> Map (Api.Hash Api.StakeKey) (a, Vote)
  -> Map (Api.Hash Api.StakeKey) (a, Vote)
accumulateRegistrations r@(_, rego) =
  let
    stakeHash :: Api.Hash Api.StakeKey
    stakeHash = voteRegistrationStakeHash rego
  in
    M.insertWith chooseNewer stakeHash r

chooseNewer
  :: Ord a
  => (a, Vote) -> (a, Vote) -> (a, Vote)
chooseNewer a b = if b `isNewer` a then b else a

-- | A newer registration will apply over an older one iff the nonce of the new
-- registration is greater than the old.
isNewer
  :: Ord a
  => (a, Vote)
  -> (a, Vote)
  -> Bool
isNewer a@(tA, _regoA) b@(tB, _regoB) =
  let
    (new, old) = if tA > tB then (a, b) else (b, a)

    slotNew = voteRegistrationSlot $ snd new
    slotOld = voteRegistrationSlot $ snd old
  in
    a == (if slotNew > slotOld then new else old)

decodeMetadata :: TxMetadata -> Either String Aeson.Object
decodeMetadata =
  Aeson.eitherDecode'
  . TL.encodeUtf8
  . TL.fromStrict
  . fromMaybe mempty
  . txMetadataJson

parseRegistration
  :: Aeson.Value
  -> Either MetadataRetrievalError Vote
parseRegistration rego = do
  voteRegoTxMetadata <-
    handleEither
    (\e -> _MetadataFailedToDecodeTxMetadata # (toSqlKey 0, e))
    $ Api.metadataFromJson Api.TxMetadataJsonNoSchema rego

  voteRego <-
    handleEither (\e -> _MetadataFailedToParseVoteRegistration # (toSqlKey 0, e))
    $ voteFromTxMetadata voteRegoTxMetadata

  pure voteRego

handleEither :: (e -> e') -> Either e x -> Either e' x
handleEither f = either (throwError . f) pure
