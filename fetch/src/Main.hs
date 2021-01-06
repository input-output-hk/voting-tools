{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Cardano.Db (DbLovelace(unDbLovelace))
import Control.Monad.Except (runExceptT, MonadError, throwError)
import Control.Monad.Reader (MonadReader, asks, runReaderT, ask)
import qualified Options.Applicative as Opt
import Database.Persist.Postgresql (SqlBackend, withPostgresqlConn, SqlPersistT, runSqlConnWithIsolation, IsolationLevel(Serializable), rawQuery, rawSql, Entity)
import Control.Monad.Logger
    ( logInfoN, runStdoutLoggingT )
import Data.Text (Text)
import Data.Traversable (forM)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Char8 as BC
import Data.Ratio (numerator)
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource
import Cardano.Db
import Database.Esqueleto
import System.IO
import qualified Data.Map.Monoidal as MM
import Data.Monoid (Sum(Sum), getSum, Last(Last), getLast, First(First), getFirst)
import Data.Maybe (fromJust)

import Cardano.API (SlotNo)
import Cardano.CLI.Voting.Metadata (Vote, AsMetadataParsingError(..), withMetaKey, fromTxMetadata, metadataMetaKey, signatureMetaKey, MetadataParsingError, voteRegistrationVerificationKey, voteRegistrationPublicKey)
import Cardano.CLI.Voting.Signing (VoteVerificationKeyHash, getVoteVerificationKeyHash, AsType(AsVoteVerificationKeyHash))
import Cardano.CLI.Fetching (Threshold, VotingFunds(VotingFunds), aboveThreshold)
import qualified Cardano.API as Api
import qualified Cardano.Api.Typed as Api (metadataFromJson)
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
import Cardano.API.Extended (VotingKeyPublic, serialiseToBech32')
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Control.Lens ((#))
import Control.Lens.TH (makeClassyPrisms)

import qualified Cardano.API.Jormungandr as Jormungandr

import           Config

type StakeHash = ()
type VotingKeys = Map StakeHash VotingKeyPublic

parseMetadataFromJson :: Aeson.Object -> Either Api.TxMetadataJsonError Api.TxMetadata
parseMetadataFromJson = Api.metadataFromJson Api.TxMetadataJsonNoSchema . Aeson.Object

-- x
--   :: ( MonadError e m
--      , AsMetadataParsingError e
--      )
--   => NetworkId
--   -> VoteMetadata
--   -> m VotingKeys
-- x nw metaJSONRaw sigJSONRaw = do
--   meta    <- either (throwError . _MetadataJSONParsingFailure metaJSONRaw) pure $ parseMetadataFromJson metaJSONRaw
--   sigMeta <- either (throwError . _MetadataJSONParsingFailure sigJSONRaw)  pure $ parseMetadataFromJson sigJSONRaw
--   (sig :: SigDSIGN) <-
--     case M.lookup 1 sigMeta of
--       Nothing     -> throwError (_MetadataMissingField # (sigMeta, 1))
--       Just sigRaw -> maybe (throwError . DeserialiseSigDSIGNFailure sigRaw) pure $ rawDeserialiseSigDSIGN sigRaw
--   (pubkey :: VerKeyDSIGN) <-
--     case M.lookup 2 meta of
--       Nothing        -> throwError (_MetadataMissingField # (metaRaw, 2))
--       Just pubkeyRaw -> maybe (throwError (FailedToDeserialise)) pure $ rawDeserialiseVerKeyDSIGN pubkeyRaw
--   if verifyDSIGN () pubkey (toCBOR meta) sig == False
--   then
--     -- pure [pubkey (toCBOR meta) sig]
--     -- log verifcation errors
--   else 
--     let
--       hash = makeShelleyAddress nw (PaymentCredentialsByKey (verificationKeyHash pubkey)) NoStakeAddress
--     in
--       M.insert hash pubkey



-- x :: Word64 -> ByteString -> Text -> Text -> VotingKeys
-- x txId txHash meta sig =

data VoteMetadata
  = VoteMetadata { _voteMetaMetadata  :: TxMetadata
                 , _voteMetaSignature :: TxMetadata
                 }

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

getVotingFunds
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
getVotingFunds nw mSlotNo = do
  regos <- queryVoteRegistration mSlotNo
  liftIO $ putStrLn $ "Voter registrations returned: " <> show (length regos)
  m <- fmap mconcat $ forM regos $ \rego -> do
    let
      verKeyHash = getVoteVerificationKeyHash . voteRegistrationVerificationKey $ rego
      votePub    = voteRegistrationPublicKey rego
    liftIO $ putStrLn $ show (Api.serialiseToRawBytesHex verKeyHash)
    liftIO $ putStrLn $ show (Api.serialiseToRawBytesHex votePub)
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
        Nothing   -> (sqlBase <> " LIMIT 100;")
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

runStakeQuery :: DatabaseConfig -> VoteVerificationKeyHash -> IO ()
runStakeQuery dbConfig stakeHash = runStdoutLoggingT $ do
  logInfoN $ T.pack $ "Connecting to database at " <> _dbHost dbConfig
  withPostgresqlConn (pgConnectionString dbConfig) $ \backend -> do
    results <- runQuery backend $ runExceptT $ queryStake Nothing stakeHash
    case results of
      Left (err :: VoteRegistrationRetrievalError) -> error $ show err
      Right stake -> liftIO $ putStrLn $ show stake

runServer :: Api.NetworkId -> DatabaseConfig -> Threshold -> IO ()
runServer networkId dbConfig threshold = runStdoutLoggingT $ do
  logInfoN $ T.pack $ "Connecting to database at " <> _dbHost dbConfig
  withPostgresqlConn (pgConnectionString dbConfig) $ \backend -> do
    (results) <-
      runQuery backend $ runExceptT $ do
        aboveThreshold threshold <$> getVotingFunds networkId Nothing
        
        -- regos <- queryVoteRegistration Nothing
        -- let verKeyHashes = (Api.serialiseToRawBytesHex . getVoteVerificationKeyHash . voteRegistrationVerificationKey) <$> regos
        -- let voteKeys = (Api.serialiseToRawBytesHex . voteRegistrationPublicKey) <$> regos
        -- pure $ zip verKeyHashes voteKeys
    case results of
      Left (err :: VoteRegistrationRetrievalError) -> error $ show err
      Right (VotingFunds xs) -> liftIO $ do
        withFile "/home/sam/code/iohk/voting-tools/test.txt" WriteMode $ \h -> do
          let m = M.toList xs
          hPutStrLn h $ show $ length m
          hPutStrLn h $ show $ fmap (\(jaddr, votingPower) -> (serialiseToBech32' jaddr, votingPower)) $ m

test = runServer Api.Mainnet dbCfg 8000000000
dbCfg = DatabaseConfig "cexplorer" "cardano-node" "/run/postgresql"

runQuery :: (MonadIO m) => SqlBackend -> SqlPersistT IO a -> m a
runQuery backend query = liftIO $ runSqlConnWithIsolation query backend Serializable

main :: IO ()
main = do
  -- Parse command-line options
  opts <- Opt.execParser opts
  putStrLn $ show opts
  eCfg  <- runExceptT $ mkConfig opts
  case eCfg of
    Left err  -> putStrLn $ show err
    Right cfg@(Config networkId threshold dbCfg slotNo extraFunds) -> do
      putStrLn $ show cfg
      runServer networkId dbCfg threshold
