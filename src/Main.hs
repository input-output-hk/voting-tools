{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Cardano.Api (Hash, Lovelace (..), StakeAddress, StakeKey, serialiseAddress)
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.Logger (logInfoN, runNoLoggingT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Function ((&))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Version (showVersion)
import           Database.Persist.Postgresql (ConnectionString, SqlPersistT, withPostgresqlConn)
import qualified Options.Applicative as Opt
import           System.Info (arch, compilerName, compilerVersion, os)

import           Cardano.CLI.Fetching (Fund, chunkFund, fundFromVotingFunds, scaleFund)
import           Cardano.CLI.Query (MetadataRetrievalError)
import qualified Cardano.CLI.Query as Query
import           Config
import qualified Config.Genesis as Genesis
import qualified Config.Rewards as Rewards
import           Genesis (decodeGenesisTemplateJSON, getBlockZeroDate, setBlockZeroDate,
                   setInitialFunds)
import           Paths_voting_tools (version)

main :: IO ()
main = do
  options <- Opt.execParser opts

  case options of
    -- Version
    Version ->
      putStrLn $ mconcat
                [ "voting-tools ", showVersion version
                , " - ", os, "-", arch
                , " - ", compilerName, "-", showVersion compilerVersion
                ]
    -- Rewards
    Rewards rOpts -> do
      let (Rewards.Config networkId threshold db slotNo (Lovelace totalRewards) outfile) = Rewards.mkConfig rOpts

      (votingProportions :: Map (StakeAddress, Hash StakeKey) Double) <-
        runQuery db $ Query.queryVotingProportion networkId slotNo threshold

      let
        raw :: [(StakeAddress, Double)]
        raw = fmap (\((rewardsAddr, _), v) -> (rewardsAddr, v)) $ M.toList votingProportions

        ofRewards :: Double -> Double
        ofRewards = ((fromIntegral totalRewards) *)

        toRewards :: [(StakeAddress, Double)] -> [(StakeAddress, Integer)]
        toRewards = fmap (\(rewardsAddr, proportion) -> (rewardsAddr, proportion & ofRewards & round))

        rewards :: [(StakeAddress, Integer)]
        rewards = toRewards raw

        toAddrLovelaceMap :: [(StakeAddress, Integer)] -> Map Text Integer
        toAddrLovelaceMap = M.fromList . fmap (\(addr, reward) -> (serialiseAddress addr, reward))

      BLC.writeFile outfile . toJSON Aeson.Generic . toAddrLovelaceMap $ rewards

    -- Genesis
    Genesis gOpts -> do
      eCfg <- runExceptT (Genesis.mkConfig gOpts)
      case eCfg of
        Left (err :: Genesis.ConfigError) ->
          fail $ show err
        Right (Genesis.Config networkId threshold scale db slotNo extraFunds outfile) -> do
          votingFunds <-
            runQuery db $ Query.queryVotingFunds networkId slotNo threshold

          let
            allFunds :: Fund
            allFunds = scaleFund scale $ fundFromVotingFunds $ votingFunds <> extraFunds

          genesisTemplate <- decodeGenesisTemplateJSON
          blockZeroDate   <- getBlockZeroDate

          let
            genesis =
              genesisTemplate
                & setInitialFunds (chunkFund 100 $ allFunds)
                & setBlockZeroDate blockZeroDate

          BLC.writeFile outfile . toJSON Aeson.Generic $ genesis

toJSON :: Aeson.ToJSON a => Aeson.NumberFormat -> a -> BLC.ByteString
toJSON numFormat = Aeson.encodePretty' (Aeson.defConfig { Aeson.confCompare = Aeson.compare, Aeson.confNumFormat = numFormat })

runQuery :: DatabaseConfig -> ExceptT MetadataRetrievalError (SqlPersistT IO) a -> IO a
runQuery dbConfig q = runNoLoggingT $ do
  logInfoN $ T.pack $ "Connecting to database at " <> _dbHost dbConfig
  withPostgresqlConn (pgConnectionString dbConfig) $ \backend -> do
    result <- Query.runQuery backend $ runExceptT $ q

    case result of
      Left (err :: MetadataRetrievalError) -> fail $ show err
      Right x                              -> pure x


pgConnectionString :: DatabaseConfig -> ConnectionString
pgConnectionString (DatabaseConfig dbName dbUser dbHost) = BSC.pack $ "host=" <> dbHost <> " dbname=" <> dbName <> " user=" <> dbUser
