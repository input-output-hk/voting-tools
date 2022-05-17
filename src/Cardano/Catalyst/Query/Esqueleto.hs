{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Catalyst.Query.Esqueleto where

import           Cardano.CLI.Voting.Metadata (MetadataParsingError, metadataMetaKey,
                   signatureMetaKey)
import           Cardano.Catalyst.Query.Types (Query)
import           Cardano.Db (Block, DbLovelace (..), DbWord64 (..), EntityField (..), Key,
                   StakeAddress, Tx, TxId, TxIn, TxMetadata, TxOut, txMetadataJson)
import           Control.Lens.TH (makeClassyPrisms)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (ReaderT)
import           Data.ByteString (ByteString)
import           Data.Foldable (foldl')
import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Sum (..))
import           Data.Text (Text)
import           Database.Esqueleto.Legacy (Entity (..), InnerJoin (..), Value (..), from,
                   isNothing, on, select, unValue, val, where_, (<=.), (==.), (?.), (^.))
import           Database.Persist.Sql (SqlBackend)

import qualified Cardano.Api as Api
import qualified Cardano.Catalyst.Query.Types as Query
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

esqlQuery
  :: MonadIO m
  => Query (ReaderT SqlBackend m) TxId
esqlQuery =
  Query.Query { Query.queryVoteRegistrations = queryVoteRegistrations
              , Query.queryStakeValue = queryStakeValue
              , Query.queryStakeValues = queryStakeValues
              }

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
    -- Given a snapshot of the txins and outs before the given slot number,
    (txOut E.:& stakeAddress E.:& txIn) <-
      E.from $ utxoSnapshot mSlotNo stakeAddrs
    -- return those that haven't been spent.
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

queryStakeValue
  :: MonadIO m
  => Maybe Api.SlotNo
  -> Api.StakeAddress
  -> ReaderT SqlBackend m Integer
queryStakeValue mSlotNo stakeAddr = do
  (values :: [Value DbLovelace]) <- E.select $ do
    -- Given a snapshot of the txins and outs before the given slot number,
    (txOut E.:& _stakeAddress E.:& txIn) <-
      E.from $ utxoSnapshot mSlotNo [stakeAddr]
    -- return those that haven't been spent.
    E.where_ (isNothing $ txIn ?. TxInTxInId)
    pure ( txOut ^. TxOutValue )

  pure $ getSum $ foldMap (Sum . fromIntegral . unDbLovelace . unValue) values

utxoSnapshot
  :: Maybe Api.SlotNo
  -> [Api.StakeAddress]
  -> E.From ( E.SqlExpr (Entity TxOut)
              E.:& E.SqlExpr (Entity StakeAddress)
              E.:& E.SqlExpr (Maybe (Entity TxIn))
     )
utxoSnapshot mSlotNo stakeAddrs =
  txOutSnapshot `E.leftJoin` txInSnapshot
    -- match TxIns & TxOuts on appropriate indices,
    `E.on` (\((txOut E.:& _) E.:& txIn) ->
              E.just (txOut ^. TxOutTxId) E.==. txIn ?. TxInTxOutId
              E.&&. E.just (txOut ^. TxOutIndex) E.==. txIn ?. TxInTxOutIndex)
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

decodeMetadata :: TxMetadata -> Either String Aeson.Object
decodeMetadata =
  Aeson.eitherDecode'
  . TL.encodeUtf8
  . TL.fromStrict
  . fromMaybe mempty
  . txMetadataJson
