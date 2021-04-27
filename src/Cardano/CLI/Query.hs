{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Cardano.CLI.Query where

import           Cardano.Db (DbLovelace (DbLovelace), EntityField (TxId), TxId)
import           Control.Monad (foldM)
import           Control.Monad.Except (MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, ask, asks, runReaderT)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Function ((&))
import           Data.List (foldl')
import           Data.Monoid (Sum (Sum), getSum)
import           Data.Ratio (Ratio, denominator, numerator)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Traversable (for, forM)
import           Data.Word (Word64)
import           Database.Esqueleto (BackendCompatible, Single (Single))
import           Database.Persist.Postgresql (Entity, IsolationLevel (Serializable), SqlBackend,
                     SqlPersistT, rawExecute, rawSql, runSqlConnWithIsolation)
import           System.IO (hPutStr, hPutStrLn, stderr)

import           Ouroboros.Network.Block (unSlotNo)

import           Cardano.Api (SlotNo)
import qualified Cardano.Api as Api
import           Cardano.API.Extended (VotingKeyPublic, serialiseToBech32')
import qualified Cardano.Api.Typed as Api (Lovelace (Lovelace),
                     StakeCredential (StakeCredentialByKey), metadataFromJson)
import           Cardano.CLI.Fetching (Fund, Threshold, VotingFunds (VotingFunds), aboveThreshold,
                     chunkFund, fundFromVotingFunds)
import           Cardano.CLI.Voting.Metadata (AsMetadataParsingError (..), MetadataParsingError,
                     Vote, metadataMetaKey, parseMetadataFromJson, signatureMetaKey,
                     voteFromTxMetadata, voteRegistrationPublicKey, voteRegistrationRewardsAddress,
                     voteRegistrationVerificationKey, withMetaKey)
import           Cardano.CLI.Voting.Signing (AsType (AsVoteVerificationKeyHash),
                     VoteVerificationKeyHash, getStakeHash, getVoteVerificationKeyHash)
import           Contribution (Contributions, causeSumAmounts, contribute, filterAmounts,
                     proportionalize)
import           Control.Lens (( # ))
import           Control.Lens.TH (makeClassyPrisms)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import qualified Cardano.API.Jormungandr as Jormungandr

data MetadataRetrievalError
  = MetadataFailedToRetrieveMetadataField
  | MetadataFailedToRetrieveSignatureField
  | MetadataFailedToDecodeMetadataField !String
  | MetadataFailedToDecodeSignatureField !String
  | MetadataFailedToDecodeTxMetadata Aeson.Object !Api.TxMetadataJsonError
  deriving (Eq, Show)

makeClassyPrisms ''MetadataRetrievalError

data MetadataError
  = MetadataErrorRetrieval !MetadataRetrievalError
  | MetadataErrorParsing !MetadataParsingError
  deriving (Eq, Show)

makeClassyPrisms ''MetadataError

instance AsMetadataRetrievalError MetadataError where
  _MetadataRetrievalError = _MetadataErrorRetrieval

instance AsMetadataParsingError MetadataError where
  _MetadataParsingError = _MetadataErrorParsing

queryStake
  :: ( MonadIO m
     , MonadReader backend m
     , BackendCompatible SqlBackend backend
     )
  => Api.NetworkId
  -> Api.Hash Api.StakeKey
  -> m Api.Lovelace
queryStake nw stakeHash = do
  r <- ask
  let
      -- TODO: figure out how to obtain this from NetworkId via cardano-api
      stakeAddress = Api.makeStakeAddress nw (Api.StakeCredentialByKey stakeHash)
      stakeAddressHex = T.pack (BC.unpack $ Api.serialiseToRawBytesHex stakeAddress)
      stakeQuerySql = "SELECT SUM(utxo_snapshot.value) FROM utxo_snapshot WHERE stake_credential = decode('" <> stakeAddressHex <> "', 'hex');"
  (stakeValues :: [Single (Maybe DbLovelace)]) <- (flip runReaderT) r $ rawSql stakeQuerySql []
  case stakeValues of
    x1:(x2:xs)                            -> error "Too many stake values found for stake sum, is SUM missing from your query?"
    []                                    -> pure 0
    (Single Nothing):[]                   -> pure 0
    (Single (Just (DbLovelace stake))):[] -> pure $ fromIntegral stake

queryVoteRegistrationInfo
  :: ( MonadIO m
     , MonadReader backend m
     , BackendCompatible SqlBackend backend
     , MonadError e m
     , AsMetadataParsingError e
     , AsMetadataRetrievalError e
     )
  => Api.NetworkId
  -> Maybe SlotNo
  -> m (Contributions VotingKeyPublic (Api.StakeAddress, Api.Hash Api.StakeKey) Integer)
queryVoteRegistrationInfo nw mSlotNo  = do
  regos <- queryVoteRegistration nw mSlotNo

  liftIO $ hPutStr stderr $ "\nRaw DB query rows returned: " <> show (length regos)

  let
    -- Each stake key has one public voting key it can stake to
    xs :: Map (Api.Hash Api.StakeKey) (VotingKeyPublic, Api.StakeAddress)
    xs = fmap snd $ foldl' (\acc (txid, rego) ->
             let
               verKeyHash = getStakeHash . voteRegistrationVerificationKey $ rego
               votePub    = voteRegistrationPublicKey rego
               rewardsAddr = voteRegistrationRewardsAddress rego
             in
               case M.lookup verKeyHash acc of
                 Nothing      -> M.insert verKeyHash (txid, (votePub, rewardsAddr)) acc
                 Just (t, _)  -> if txid >= t
                                 then M.insert verKeyHash (txid, (votePub, rewardsAddr)) acc
                                 else acc
           ) mempty regos

  liftIO $ hPutStrLn stderr $ "\nKeys eligible: " <> show (length xs)

  let
    xs' = zip [1..] (M.toList xs)
  stakeTempTableSql <- case mSlotNo of
    Nothing     ->
      pure "CREATE TEMPORARY TABLE IF NOT EXISTS utxo_snapshot AS (SELECT tx_out.*, stake_address.hash_raw AS stake_credential FROM tx_out LEFT OUTER JOIN tx_in ON tx_out.tx_id = tx_in.tx_out_id AND tx_out.index = tx_in.tx_out_index INNER JOIN stake_address ON stake_address.id = tx_out.stake_address_id WHERE tx_in.tx_in_id IS NULL);"
    Just slotNo -> do
      let tx_out_snapshot = "CREATE TEMPORARY TABLE IF NOT EXISTS tx_out_snapshot AS (SELECT tx_out.*, stake_address.hash_raw AS stake_credential FROM tx_out INNER JOIN tx ON tx_out.tx_id = tx.id INNER JOIN block ON tx.block_id = block.id INNER JOIN stake_address ON stake_address.id = tx_out.stake_address_id WHERE block.slot_no <= " <> T.pack (show $ unSlotNo slotNo) <> "); "
          tx_in_snapshot = "CREATE TEMPORARY TABLE IF NOT EXISTS tx_in_snapshot AS (SELECT tx_in.* FROM tx_in INNER JOIN tx ON tx_in.tx_in_id = tx.id INNER JOIN block ON tx.block_id = block.id WHERE block.slot_no <= " <> T.pack (show $ unSlotNo slotNo) <> "); "
          utxo_snapshot = "CREATE TEMPORARY TABLE IF NOT EXISTS utxo_snapshot AS (SELECT tx_out_snapshot.* FROM tx_out_snapshot LEFT OUTER JOIN tx_in_snapshot ON tx_out_snapshot.tx_id = tx_in_snapshot.tx_out_id AND tx_out_snapshot.index = tx_in_snapshot.tx_out_index WHERE tx_in_snapshot.tx_in_id IS NULL);"
      pure $ tx_out_snapshot <> tx_in_snapshot <> utxo_snapshot
  r <- ask

  _ :: () <- (flip runReaderT) r $ rawExecute stakeTempTableSql []
  foldM (\acc (idx, (verKeyHash, (votepub, rewardsAddr))) -> do
    (Api.Lovelace stake) <- queryStake nw verKeyHash

    liftIO $ hPutStr stderr $ "\rProcessing vote stake " <> show idx <> " of " <> show (length xs)
    pure $ contribute votepub (rewardsAddr, verKeyHash) stake acc
        ) mempty xs'

queryVotingProportion
  :: ( MonadIO m
     , MonadReader backend m
     , BackendCompatible SqlBackend backend
     , MonadError e m
     , AsMetadataParsingError e
     , AsMetadataRetrievalError e
     )
  => Api.NetworkId
  -> Maybe SlotNo
  -> Threshold
  -> m (Map (Api.StakeAddress, Api.Hash Api.StakeKey) Double)
queryVotingProportion nw mSlotNo (Api.Lovelace threshold) = do
  info <- filterAmounts (> threshold) <$> queryVoteRegistrationInfo nw mSlotNo

  let
    proportions :: [((Api.StakeAddress, Api.Hash Api.StakeKey), Double)]
    proportions =
      proportionalize info
      & foldMap snd
      & fmap (fmap convertRatio)

    convertRatio :: Ratio Integer -> Double
    convertRatio ratio = (fromIntegral $ numerator ratio) / (fromIntegral $ denominator ratio)

  pure $ M.fromList proportions

queryVotingFunds
  :: ( MonadIO m
     , MonadReader backend m
     , BackendCompatible SqlBackend backend
     , MonadError e m
     , AsMetadataParsingError e
     , AsMetadataRetrievalError e
     )
  => Api.NetworkId
  -> Maybe SlotNo
  -> Threshold
  -> m VotingFunds
queryVotingFunds nw mSlotNo (Api.Lovelace threshold) = do
  info <- queryVoteRegistrationInfo nw mSlotNo

  let info' = filterAmounts (> threshold) info

  pure
    $ VotingFunds
    $ M.fromList
    $ fmap (\(votepub, amt) ->
              ( Jormungandr.addressFromVotingKeyPublic nw votepub
              , Api.Lovelace $ numerator amt
              )
           )
    $ causeSumAmounts info'

queryVoteRegistration
  :: ( MonadIO m
     , MonadError e m
     , AsMetadataParsingError e
     , AsMetadataRetrievalError e
     , MonadReader backend m
     , BackendCompatible SqlBackend backend
     )
  => Api.NetworkId
  -> Maybe SlotNo
  -> m [(TxId, Vote)]
queryVoteRegistration nw mSlotNo =
  let
    sqlBase = "WITH meta_table AS (select tx_id, json AS metadata from tx_metadata where key = '" <> T.pack (show metadataMetaKey) <> "') , sig_table AS (select tx_id, json AS signature from tx_metadata where key = '" <> T.pack (show signatureMetaKey) <> "') SELECT tx.hash,tx_id,metadata,signature FROM meta_table INNER JOIN tx ON tx.id = meta_table.tx_id INNER JOIN sig_table USING(tx_id)"
  in do
    let
      sql = case mSlotNo of
        Just slot -> (sqlBase <> "INNER JOIN block ON block.id = tx.block_id WHERE block.slot_no <= " <> T.pack (show $ unSlotNo slot) <> ";")
        Nothing   -> (sqlBase <> ";")
    r <- ask
    (results :: [(Single ByteString, Single TxId, Single (Maybe Text), Single (Maybe Text))]) <- (flip runReaderT) r $ rawSql sql []
    fmap mconcat $ forM results $ \(Single txHash, Single txId, Single mMetadata, Single mSignature) -> do
      metadata  <- maybe (throwError $ _MetadataFailedToRetrieveMetadataField # ()) pure mMetadata
      signature <- maybe (throwError $ _MetadataFailedToRetrieveSignatureField # ()) pure mSignature

      metadataObj <- either (throwError . (_MetadataFailedToDecodeMetadataField #)) pure $ Aeson.eitherDecode' $ TL.encodeUtf8 $ TL.fromStrict $ metadata
      signatureObj <- either (throwError . (_MetadataFailedToDecodeSignatureField #)) pure $ Aeson.eitherDecode' $ TL.encodeUtf8 $ TL.fromStrict $ signature

      let
        metaObj :: Aeson.Object
        metaObj = HM.fromList
          [ (T.pack $ show metadataMetaKey, metadataObj)
          , (T.pack $ show signatureMetaKey, signatureObj)
          ]

      meta <- either (\err -> throwError $ (_MetadataFailedToDecodeTxMetadata # (metaObj, err))) pure $ parseMetadataFromJson (Aeson.Object metaObj)
      -- We now ignore any Metadata that fails to decode (generally will be due to entries missing the new "3" metadata field)
      either (const $ pure []) (\v -> pure [v]) $ fmap (txId,) $ voteFromTxMetadata meta

runQuery :: (MonadIO m) => SqlBackend -> SqlPersistT IO a -> m a
runQuery backend query = liftIO $ runSqlConnWithIsolation query backend Serializable

