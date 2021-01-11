{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Cardano.CLI.Query where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (runExceptT, MonadError, throwError)
import Control.Monad.Reader (MonadReader, asks, runReaderT, ask)
import Database.Persist.Postgresql (SqlBackend, SqlPersistT, runSqlConnWithIsolation, IsolationLevel(Serializable), rawSql, Entity)
import Data.Text (Text)
import Data.Traversable (forM, for)
import Data.Word (Word64)
import Data.List (foldl')
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Char8 as BC
import Cardano.Db (DbLovelace(DbLovelace), EntityField(TxId), TxId)
import Database.Esqueleto (BackendCompatible, Single(Single))
import qualified Data.Map.Monoidal as MM
import Data.Monoid (Sum(Sum), getSum)

import Cardano.API (SlotNo)
import Cardano.CLI.Voting.Metadata (Vote, AsMetadataParsingError(..), withMetaKey, fromTxMetadata, metadataMetaKey, signatureMetaKey, MetadataParsingError, voteRegistrationVerificationKey, voteRegistrationPublicKey)
import Cardano.CLI.Voting.Signing (VoteVerificationKeyHash, getVoteVerificationKeyHash, AsType(AsVoteVerificationKeyHash))
import Cardano.CLI.Fetching (Threshold, Fund, VotingFunds(VotingFunds), aboveThreshold, fundFromVotingFunds, chunkFund)
import qualified Cardano.API as Api
import qualified Cardano.Api.Typed as Api (metadataFromJson)
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as Aeson
import Control.Lens ((#))
import Control.Lens.TH (makeClassyPrisms)
import Registration (register, registry)

import qualified Cardano.API.Jormungandr as Jormungandr

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

-- | A simple helper type, takes two pieces of data and provides an
-- Ord instance that only compares the first piece of data.
data OrderedBy ord a = OrderedBy ord a

instance Eq ord => Eq (OrderedBy ord a) where
  (==) (OrderedBy ord1 _) (OrderedBy ord2 _) = ord1 == ord2

instance Ord ord => Ord (OrderedBy ord a) where
  compare (OrderedBy ord1 _) (OrderedBy ord2 _) = compare ord1 ord2

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
  let
    addRego registry (txId, rego) =
      let
        verKeyHash = getVoteVerificationKeyHash . voteRegistrationVerificationKey $ rego
        votePub    = voteRegistrationPublicKey rego
      in
        register verKeyHash (OrderedBy txId votePub) registry

  -- By the Registration algebra, registrations are naturally filtered
  -- to only use the latest registration for each verification key.
  let latestRegistrations = foldl' addRego mempty regos

  addrStakeMap <- fmap mconcat $ forM (registry latestRegistrations) $ \(verKeyHash, (OrderedBy _txId votePub)) -> do
    stake <- queryStake mSlotNo verKeyHash
    pure $ MM.singleton (Jormungandr.addressFromVotingKeyPublic nw votePub) (Sum stake)

  pure . VotingFunds . M.fromList . MM.toList . fmap getSum $ addrStakeMap

queryVoteRegistration
  :: ( MonadIO m
     , MonadError e m
     , AsMetadataParsingError e
     , AsMetadataRetrievalError e
     , MonadReader backend m
     , BackendCompatible SqlBackend backend
     )
  => Maybe SlotNo
  -> m [(TxId, Vote)]
queryVoteRegistration mSlotNo =
  let
    sqlBase = "WITH meta_table AS (select tx_id, json AS metadata from tx_metadata where key = '" <> T.pack (show metadataMetaKey) <> "') , sig_table AS (select tx_id, json AS signature from tx_metadata where key = '" <> T.pack (show signatureMetaKey) <> "') SELECT tx.hash,tx_id,metadata,signature FROM meta_table INNER JOIN tx ON tx.id = meta_table.tx_id INNER JOIN sig_table USING(tx_id)"
  in do
    let
      sql = case mSlotNo of
        Just slot -> (sqlBase <> "INNER JOIN block ON block.id = tx.block_id WHERE block.slot_no < " <> T.pack (show slot) <> ";")
        Nothing   -> (sqlBase <> ";")
    r <- ask
    (results :: [(Single ByteString, Single TxId, Single (Maybe Text), Single (Maybe Text))]) <- (flip runReaderT) r $ rawSql sql []
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
      either (throwError . (_MetadataParsingError #)) pure $ fmap (txId,) $ fromTxMetadata meta

runQuery :: (MonadIO m) => SqlBackend -> SqlPersistT IO a -> m a
runQuery backend query = liftIO $ runSqlConnWithIsolation query backend Serializable

