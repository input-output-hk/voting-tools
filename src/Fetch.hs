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
    ( logInfoN, runStdoutLoggingT, runStderrLoggingT, runNoLoggingT)
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
import Database.Esqueleto (BackendCompatible, Single(Single))
import System.IO
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Map.Monoidal as MM
import Data.Monoid (Sum(Sum), getSum, Last(Last), getLast, First(First), getFirst)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8 as BLC

import Cardano.API (SlotNo)
import Cardano.CLI.Voting.Metadata (Vote, AsMetadataParsingError(..), withMetaKey, fromTxMetadata, metadataMetaKey, signatureMetaKey, MetadataParsingError, voteRegistrationVerificationKey, voteRegistrationPublicKey)
import Cardano.CLI.Query
import Cardano.CLI.Voting.Signing (VoteVerificationKeyHash, getVoteVerificationKeyHash, AsType(AsVoteVerificationKeyHash))
import Cardano.CLI.Fetching (Threshold, Fund, VotingFunds(VotingFunds), aboveThreshold, fundFromVotingFunds, chunkFund)
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

import           Config.Fetch

import Control.Lens
import Data.Aeson.Lens

main :: IO ()
main = do
  opts <- Opt.execParser opts
  eCfg  <- runExceptT $ mkConfig opts
  case eCfg of
    Left err ->
      putStrLn $ show err
    Right cfg@(Config networkId threshold dbCfg slotNo extraFunds) -> do
      -- p <- getVotingProportion networkId dbCfg threshold slotNo
      -- putStrLn $ show p
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
        queryVotingFunds networkId slotNo threshold
    case results of
      Left (err :: VoteRegistrationRetrievalError) -> error $ show err
      Right (votingFunds) -> pure votingFunds

getVotingProportion :: Api.NetworkId -> DatabaseConfig -> Threshold -> Maybe SlotNo -> IO (Map (Api.Hash Api.StakeKey) Double)
getVotingProportion networkId dbConfig threshold slotNo = runNoLoggingT $ do
  logInfoN $ T.pack $ "Connecting to database at " <> _dbHost dbConfig
  withPostgresqlConn (pgConnectionString dbConfig) $ \backend -> do
    (results) <-
      runQuery backend $ runExceptT $ do
        queryVotingProportion networkId slotNo threshold
    case results of
      Left (err :: VoteRegistrationRetrievalError) -> error $ show err
      Right proportion -> pure proportion

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
