{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (runExceptT)
import qualified Options.Applicative as Opt
import Database.Persist.Postgresql (SqlBackend, withPostgresqlConn, SqlPersistT, runSqlConnWithIsolation, IsolationLevel(Serializable), rawQuery, rawSql, Entity)
import Control.Monad.Logger
    ( logInfoN, runStdoutLoggingT )
import Data.Text (Text)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource
import Cardano.Db
import Database.Esqueleto

import qualified Cardano.API as Api
import qualified Cardano.Api.Typed as Api (metadataFromJson)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Cardano.API.Extended (VotingKeyPublic)
import qualified Data.Aeson as Aeson

import           Config

type StakeHash = ()
type VotingKeys = Map StakeHash VotingKeyPublic

parseMetadataFromJson :: Text -> Either String Api.TxMetadata
parseMetadataFromJson = Aeson.runParser (Aeson.withObject (Api.metadataFromJson Api.TxMetadataJsonDetailedSchema))

x
  :: ( MonadError e m
     , AsMetadataParsingError e
     )
  => NetworkId
  -> VoteMetadata
  -> m VotingKeys
x nw metaJSONRaw sigJSONRaw =
  meta    <- either (throwError . _MetadataJSONParsingFailure metaJSONRaw) pure $ parseMetadataFromJson metaJSONRaw
  sigMeta <- either (throwError . _MetadataJSONParsingFailure sigJSONRaw)  pure $ parseMetadataFromJson sigJSONRaw
  (sig :: SigDSIGN) <-
    case M.lookup 1 sigMeta of
      Nothing     -> throwError (_MetadataMissingField # (sigMeta, 1))
      Just sigRaw -> maybe (throwError . DeserialiseSigDSIGNFailure sigRaw) pure $ rawDeserialiseSigDSIGN sigRaw
  (pubkey :: VerKeyDSIGN) <-
    case M.lookup 2 meta of
      Nothing        -> throwError (_MetadataMissingField # (metaRaw, 2))
      Just pubkeyRaw -> maybe (throwError (FailedToDeserialise)) pure $ rawDeserialiseVerKeyDSIGN pubkeyRaw
  if verifyDSIGN () pubkey (toCBOR meta) sig == False
  then
    -- pure [pubkey (toCBOR meta) sig]
    -- log verifcation errors
  else 
    let
      hash = makeShelleyAddress nw (PaymentCredentialsByKey (verificationKeyHash pubkey)) NoStakeAddress
    in
      M.insert hash pubkey



-- x :: Word64 -> ByteString -> Text -> Text -> VotingKeys
-- x txId txHash meta sig =

data VoteMetadata
  = VoteMetadata { _voteMetaMetadata  :: TxMetadata
                 , _voteMetaSignature :: TxMetadata
                 }

queryVoteMetaData
  :: ( MonadIO m
     , MonadError e m
     , AsMetadataParsingError e
     )
  => Maybe SlotNo
  -> m [Vote]
queryVoteMetaData mSlotNo =
  let
    metadataKey = 61284
    signatureKey = 61285
    sqlBase = "WITH meta_table AS (select tx_id, json AS metadata from tx_metadata where key = '" <> show metadataKey <> "') , sig_table AS (select tx_id, json AS signature from tx_metadata where key = '" <> show signatureKey <> "') SELECT tx.hash,tx_id,metadata,signature FROM meta_table INNER JOIN tx ON tx.id = meta_table.tx_id INNER JOIN sig_table USING(tx_id)"
  in do
    results <- case mSlotNo of
      Just slot -> rawSql (sqlBase <> "INNER JOIN block ON block.id = tx.block_id WHERE block.slot_no < " <> show slotNo <> ";") []
      Nothing   -> rawSql (sqlBase <> ";") []
    forM results $ \(Single txHash, Single txId, Single mMetadata, Single mSignature) ->
      metadataObj :: Eiter String Aeson.Object
      metadataObj = fmap withMetaKey metadataMetaKey $ eitherDecode' $ fromMaybe "" mSignature
      metadataObj :: Eiter String Aeson.Object
      signatureObj = fmap withMetaKey signatureMetaKey $ eitherDecode' $ fromMaybe "" mSignature

      meta <- parseMetadataFromJson (metadataObj <> signatureObj)
      fromTxMetadata meta

runServer :: DatabaseConfig -> IO ()
runServer dbConfig = runStdoutLoggingT $ do
  logInfoN $ T.pack $ "Connecting to database at " <> _dbSocketPath dbConfig
  withPostgresqlConn (pgConnectionString dbConfig) $ \backend -> do
    (results :: [(Single ByteString, Single Word64, Single (Maybe Text), Single (Maybe Text))]) <-
      runQuery backend $ do
        rawSql "WITH meta_table AS (select tx_id, json AS metadata from tx_metadata where key = '61284') , sig_table AS (select tx_id, json AS signature from tx_metadata where key = '61285') SELECT tx.hash,tx_id,metadata,signature FROM meta_table INNER JOIN tx ON tx.id = meta_table.tx_id INNER JOIN sig_table USING(tx_id);" []
    liftIO $ print results

runQuery :: MonadIO m => SqlBackend -> SqlPersistT IO a -> m a
runQuery backend query =
  liftIO $ runSqlConnWithIsolation query backend Serializable

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
      runServer dbCfg
