{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

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
import           Data.Conduit (($$))
import qualified Data.Conduit.List as CL
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
import           Cardano.CLI.Query
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

import           Config.Fetch

import           Control.Lens
import           Data.Aeson.Lens

main :: IO ()
main = do
  opts <- Opt.execParser opts
  eCfg  <- runExceptT $ mkConfig opts
  case eCfg of
    Left err ->
      putStrLn $ show err
    Right cfg@(Config networkId threshold dbCfg slotNo extraFunds) -> do
      (votingFunds :: VotingFunds) <- getVotingFunds networkId dbCfg threshold slotNo
      let
        allFunds :: Fund
        allFunds = fundFromVotingFunds $ votingFunds <> extraFunds

      genesisTemplate <- decodeGenesisTemplateJSON
      blockZeroDate   <- getBlockZeroDate

      let
        genesis =
          genesisTemplate
            & setInitialFunds (chunkFund 100 $ allFunds)
            & setBlockZeroDate blockZeroDate

      BLC.putStr $ Aeson.encode genesis

getVotingFunds :: Api.NetworkId -> DatabaseConfig -> Threshold -> Maybe SlotNo -> IO VotingFunds
getVotingFunds networkId dbConfig threshold slotNo = runNoLoggingT $ do
  logInfoN $ T.pack $ "Connecting to database at " <> _dbHost dbConfig
  withPostgresqlConn (pgConnectionString dbConfig) $ \backend -> do
    (results) <-
      runQuery backend $ runExceptT $ do
        aboveThreshold threshold <$> queryVotingFunds networkId slotNo
    case results of
      Left (err :: VoteRegistrationRetrievalError) -> error $ show err
      Right (votingFunds) -> pure votingFunds


decodeGenesisTemplateJSON :: IO (Aeson.Value)
decodeGenesisTemplateJSON = do
  result <- Aeson.eitherDecodeFileStrict' "genesis-template.json"
  case result of
    Left err                               -> error err
    Right (genesisTemplate :: Aeson.Value) -> pure genesisTemplate

getBlockZeroDate :: IO UTCTime
getBlockZeroDate = do
  (UTCTime day dayTime) <- getCurrentTime
  let
    (TimeOfDay hr min sec) = timeToTimeOfDay dayTime
    blockZeroDate          = UTCTime day (timeOfDayToTime $ TimeOfDay hr 0 0)
  pure blockZeroDate

setInitialFunds :: [Fund] -> Aeson.Value -> Aeson.Value
setInitialFunds funds = _Object %~ HM.insert "initial" (Aeson.toJSON funds)

setBlockZeroDate :: UTCTime -> Aeson.Value -> Aeson.Value
setBlockZeroDate time =
  (key "blockchain_configuration"
    %~ (key "block0_date"
      .~ (Aeson.Number . fromIntegral . floor . utcTimeToPOSIXSeconds $ time)))
