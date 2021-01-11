{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.CLI.Query where

import           Cardano.Db (DbLovelace (unDbLovelace))
import           Cardano.Db
import           Control.Monad.Except (MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (logInfoN, runNoLoggingT, runStderrLoggingT,
                     runStdoutLoggingT)
import           Control.Monad.Reader (MonadReader, ask, asks, runReaderT)
import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Monoidal as MM
import           Data.Maybe (fromMaybe)
import           Data.Maybe (fromJust)
import           Data.Monoid (First (First), Last (Last), Sum (Sum), getFirst, getLast, getSum)
import           Data.Ratio (numerator)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Time
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Traversable (forM)
import           Data.Word (Word64)
import           Database.Esqueleto (BackendCompatible, Single (Single))
import           Database.Persist.Postgresql (Entity, IsolationLevel (Serializable), SqlBackend,
                     SqlPersistT, rawQuery, rawSql, runSqlConnWithIsolation, withPostgresqlConn)
import qualified Options.Applicative as Opt
import           System.IO

import           Cardano.API (SlotNo)
import qualified Cardano.API as Api
import           Cardano.API.Extended (VotingKeyPublic, serialiseToBech32')
import qualified Cardano.Api.Typed as Api (metadataFromJson)
import           Cardano.CLI.Fetching (Fund, Threshold, VotingFunds (VotingFunds), aboveThreshold,
                     chunkFund, fundFromVotingFunds)
import           Cardano.CLI.Voting.Metadata (AsMetadataParsingError (..), MetadataParsingError,
                     Vote, fromTxMetadata, metadataMetaKey, signatureMetaKey,
                     voteRegistrationPublicKey, voteRegistrationVerificationKey, withMetaKey)
import           Cardano.CLI.Voting.Signing (AsType (AsVoteVerificationKeyHash),
                     VoteVerificationKeyHash, getVoteVerificationKeyHash)
import           Control.Lens (( # ))
import           Control.Lens.TH (makeClassyPrisms)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import qualified Cardano.API.Jormungandr as Jormungandr

import           Control.Lens

parseMetadataFromJson :: Aeson.Object -> Either Api.TxMetadataJsonError Api.TxMetadata
parseMetadataFromJson = Api.metadataFromJson Api.TxMetadataJsonNoSchema . Aeson.Object

data MetadataRetrievalError
  = MetadataFailedToRetrieveMetadataField
  | MetadataFailedToRetrieveSignatureField
  | MetadataFailedToDecodeMetadataField !String
  | MetadataFailedToDecodeSignatureField !String
  | MetadataFailedToDecodeTxMetadata Aeson.Object !Api.TxMetadataJsonError
  deriving (Eq, Show)

makeClassyPrisms ''MetadataRetrievalError

data VoteRegistrationRetrievalError
  = VoteRegistrationMetadataRetrievalError !MetadataRetrievalError
  | VoteRegistrationMetadataParsingError !MetadataParsingError
  deriving (Eq, Show)

makeClassyPrisms ''VoteRegistrationRetrievalError

instance AsMetadataRetrievalError VoteRegistrationRetrievalError where
  _MetadataRetrievalError = _VoteRegistrationMetadataRetrievalError

instance AsMetadataParsingError VoteRegistrationRetrievalError where
  _MetadataParsingError = _VoteRegistrationMetadataParsingError

queryStake
  :: ( MonadIO m
     , MonadReader backend m
     , BackendCompatible SqlBackend backend
     )
  => Maybe SlotNo
  -> VoteVerificationKeyHash
  -> m Api.Lovelace
queryStake mSlotRestriction stakeHash = do
  r <- ask
  let stkHashSql = T.pack (BC.unpack $ Api.serialiseToRawBytesHex stakeHash)
  stakeQuerySql <- case mSlotRestriction of
    Nothing             ->
      pure $ "SELECT SUM(utxo_view.value) FROM utxo_view WHERE CAST(encode(address_raw, 'hex') AS text) LIKE '%" <> stkHashSql <> "';"
    Just slotNo -> do
      -- Get first tx is slot after one we've asked to restrict the
      -- query to, we don't want to get this Tx or any later Txs.
      let
        firstUnwantedTxIdSql = "SELECT tx.id FROM tx LEFT OUTER JOIN block ON block.id = tx.block_id WHERE block.slot_no > " <> T.pack (show slotNo) <> " LIMIT 1";

      (txids :: [Single Word64]) <- (flip runReaderT) r $ rawSql firstUnwantedTxIdSql []
      case txids of
        []               -> error $ "No txs found in slot " <> show (slotNo + 1)-- TODO throwError (_NoTxsFoundInSlot # (slotNo + 1))
        x1:(x2:xs)       -> error $ "Too many txs found for slot " <> show slotNo <> ", add 'LIMIT 1' to query."
        (Single txid):[] ->
          pure $ "SELECT SUM(tx_out.value) FROM tx_out INNER JOIN tx ON tx.id = tx_out.tx_id LEFT OUTER JOIN tx_in ON tx_out.tx_id = tx_in.tx_out_id AND tx_out.index = tx_in.tx_out_index AND tx_in_id < " <> T.pack (show txid) <> " INNER JOIN block ON block.id = tx.block_id AND block.slot_no < " <> T.pack (show slotNo) <> " WHERE CAST(encode(address_raw, 'hex') AS text) LIKE '%" <> stkHashSql <> "' AND tx_in.tx_in_id is null;"

  (stakeValues :: [Single (Maybe DbLovelace)]) <- (flip runReaderT) r $ rawSql stakeQuerySql []
  case stakeValues of
    x1:(x2:xs)                            -> error "Too many stake values found for stake sum, is SUM missing from your query?"
    []                                    -> pure 0
    (Single Nothing):[]                   -> pure 0
    (Single (Just (DbLovelace stake))):[] -> pure $ fromIntegral stake

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
  -> m VotingFunds
queryVotingFunds nw mSlotNo = do
  regos <- queryVoteRegistration mSlotNo
  m <- fmap mconcat $ forM regos $ \rego -> do
    let
      verKeyHash = getVoteVerificationKeyHash . voteRegistrationVerificationKey $ rego
      votePub    = voteRegistrationPublicKey rego
    pure $ MM.singleton verKeyHash (Last $ Just votePub)

  mmap <- fmap mconcat $ forM (MM.toList m) $ \(verKeyHash, votePub) -> do
    stake <- queryStake mSlotNo verKeyHash
    pure $ MM.singleton (Jormungandr.addressFromVotingKeyPublic nw (fromJust $ getLast votePub)) (Sum stake)

  pure . VotingFunds . M.fromList . MM.toList . fmap getSum $ mmap

queryVoteRegistration
  :: ( MonadIO m
     , MonadError e m
     , AsMetadataParsingError e
     , AsMetadataRetrievalError e
     , MonadReader backend m
     , BackendCompatible SqlBackend backend
     )
  => Maybe SlotNo
  -> m [Vote]
queryVoteRegistration mSlotNo =
  let
    sqlBase = "WITH meta_table AS (select tx_id, json AS metadata from tx_metadata where key = '" <> T.pack (show metadataMetaKey) <> "') , sig_table AS (select tx_id, json AS signature from tx_metadata where key = '" <> T.pack (show signatureMetaKey) <> "') SELECT tx.hash,tx_id,metadata,signature FROM meta_table INNER JOIN tx ON tx.id = meta_table.tx_id INNER JOIN sig_table USING(tx_id)"
  in do
    let
      sql = case mSlotNo of
        Just slot -> (sqlBase <> "INNER JOIN block ON block.id = tx.block_id WHERE block.slot_no < " <> T.pack (show slot) <> ";")
        Nothing   -> (sqlBase <> ";")
    r <- ask
    (results :: [(Single ByteString, Single Word64, Single (Maybe Text), Single (Maybe Text))]) <- (flip runReaderT) r $ rawSql sql []
    forM results $ \(Single txHash, Single txId, Single mMetadata, Single mSignature) -> do
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

      meta <- either (\err -> throwError $ (_MetadataFailedToDecodeTxMetadata # (metaObj, err))) pure $ parseMetadataFromJson metaObj
      either (throwError . (_MetadataParsingError #)) pure $ fromTxMetadata meta

runQuery :: (MonadIO m) => SqlBackend -> SqlPersistT IO a -> m a
runQuery backend query = liftIO $ runSqlConnWithIsolation query backend Serializable

