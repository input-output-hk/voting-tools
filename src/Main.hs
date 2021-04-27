{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Cardano.Api (StakeAddress, Certificate, ShelleyBasedEra (ShelleyBasedEraShelley),
                     makeMIRCertificate, serialiseAddress, serialiseToCBOR)
import           Cardano.Api.Protocol (Protocol (CardanoProtocol), withlocalNodeConnectInfo)
import           Cardano.Api.Typed (AsType (AsStakeAddress), Hash, Lovelace (Lovelace),
                     StakeCredential (StakeCredentialByKey), StakeKey, makeStakeAddress,
                     serialiseToBech32, serialiseToRawBytesHex)
import           Cardano.Chain.Slotting (EpochSlots (..))
import           Cardano.CLI.Types (QueryFilter (FilterByAddress), SocketPath (SocketPath))
import qualified Cardano.Crypto.DSIGN as Crypto
import           Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (logInfoN, runNoLoggingT, runStderrLoggingT,
                     runStdoutLoggingT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Foldable (forM_)
import           Data.Function ((&))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Persist.Postgresql (ConnectionString)
import           Database.Persist.Postgresql (Entity, IsolationLevel (Serializable), SqlBackend,
                     SqlPersistT, rawQuery, rawSql, runSqlConnWithIsolation, withPostgresqlConn)
import qualified Options.Applicative as Opt
import           Shelley.Spec.Ledger.TxBody (MIRPot (..))
import           System.Directory (createDirectoryIfMissing)

import           Cardano.API.Extended (readEnvSocketPath)
import           Cardano.CLI.Fetching (Fund, chunk, chunkFund, fundFromVotingFunds, scaleFund)
import           Cardano.CLI.Query (MetadataError)
import qualified Cardano.CLI.Query as Query
import           Cardano.CLI.Voting (createVoteRegistration, encodeVoteRegistration, prettyTx,
                     signTx)
import           Cardano.CLI.Voting.Error (AppError)
import           Cardano.CLI.Voting.Metadata (voteSignature)
import           Cardano.CLI.Voting.Signing (verificationKeyRawBytes)
import           Config
import qualified Config.Genesis as Genesis
import qualified Config.Registration as Register
import qualified Config.Rewards as Rewards
import           Genesis (decodeGenesisTemplateJSON, getBlockZeroDate, setBlockZeroDate,
                     setInitialFunds)

main :: IO ()
main = do
  options <- Opt.execParser opts

  case options of
    -- Rewards
    Rewards rOpts -> do
      let (Rewards.Config networkId threshold db slotNo (Lovelace totalRewards) outfile) = Rewards.mkConfig rOpts

      (votingProportions :: Map (StakeAddress, Hash StakeKey) Double) <-
        runQuery db $ Query.queryVotingProportion networkId slotNo threshold

      let
        raw :: [(StakeAddress, Double)]
        raw = fmap (\((rewardsAddr, stkHash), v) -> (rewardsAddr, v)) $ M.toList votingProportions

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

runQuery :: DatabaseConfig -> ExceptT MetadataError (SqlPersistT IO) a -> IO a
runQuery dbConfig q = runNoLoggingT $ do
  logInfoN $ T.pack $ "Connecting to database at " <> _dbHost dbConfig
  withPostgresqlConn (pgConnectionString dbConfig) $ \backend -> do
    result <- Query.runQuery backend $ runExceptT $ q

    case result of
      Left (err :: MetadataError) -> fail $ show err
      Right x                     -> pure x


pgConnectionString :: DatabaseConfig -> ConnectionString
pgConnectionString (DatabaseConfig dbName dbUser dbHost) = BSC.pack $ "host=" <> dbHost <> " dbname=" <> dbName <> " user=" <> dbUser
