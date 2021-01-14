{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Except (runExceptT, MonadError, throwError, ExceptT)
import qualified Options.Applicative as Opt
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.Logger
    ( logInfoN, runStdoutLoggingT, runStderrLoggingT, runNoLoggingT)
import Database.Persist.Postgresql (SqlBackend, withPostgresqlConn, SqlPersistT, runSqlConnWithIsolation, IsolationLevel(Serializable), rawQuery, rawSql, Entity)
import Data.Function ((&))
import           Cardano.CLI.Types (QueryFilter (FilterByAddress), SocketPath (SocketPath))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base16 as Base16
import qualified Cardano.Crypto.DSIGN as Crypto
import           Cardano.Api.Protocol (Protocol (CardanoProtocol), withlocalNodeConnectInfo)
import           Cardano.Chain.Slotting (EpochSlots (..))
import           Cardano.API (ShelleyBasedEra (ShelleyBasedEraShelley))
import           Control.Monad.IO.Class (liftIO)
import Cardano.Api.Typed (serialiseToRawBytesHex, Lovelace(Lovelace), Hash, StakeKey, makeStakeAddress, StakeCredential(StakeCredentialByKey), serialiseToBech32, AsType(AsStakeAddress))

import Config
import Genesis (decodeGenesisTemplateJSON, getBlockZeroDate, setInitialFunds, setBlockZeroDate)
import qualified Config.Rewards as Rewards
import qualified Config.Genesis as Genesis
import qualified Config.Registration as Register
import Cardano.CLI.Query (MetadataError)
import qualified Cardano.CLI.Query as Query
import Cardano.CLI.Fetching (Fund, fundFromVotingFunds, chunkFund)
import           Cardano.API.Extended (readEnvSocketPath)
import           Cardano.CLI.Voting (createVote, encodeVote, prettyTx, signTx)
import           Cardano.CLI.Voting.Metadata (voteSignature)
import           Cardano.CLI.Voting.Signing (verificationKeyRawBytes)
import           Cardano.CLI.Voting.Error (AppError)

main :: IO ()
main = do
  options <- Opt.execParser opts

  case options of
    -- Rewards
    Rewards rOpts -> do
      let (Rewards.Config networkId threshold db slotNo (Lovelace totalRewards) outfile) = Rewards.mkConfig rOpts

      (votingProportions :: Map (Hash StakeKey) Double) <-
        runQuery db $ Query.queryVotingProportion networkId slotNo threshold

      let
        raw :: [(Hash StakeKey, Double)]
        raw = M.toList votingProportions

        ofRewards :: Double -> Double
        ofRewards = ((fromIntegral totalRewards) *)

        toBech32Addr :: Hash StakeKey -> Text
        toBech32Addr = serialiseToBech32 . makeStakeAddress networkId . StakeCredentialByKey

        toRewards :: [(Hash StakeKey, Double)] -> Map Text Double
        toRewards = M.fromList . fmap (\(hash, proportion) -> (toBech32Addr hash, ofRewards proportion))

      BLC.writeFile outfile . toJSON . toRewards $ raw

    -- Genesis
    Genesis gOpts -> do
      eCfg <- runExceptT (Genesis.mkConfig gOpts)
      case eCfg of
        Left (err :: Genesis.ConfigError) ->
          fail $ show err
        Right (Genesis.Config networkId threshold db slotNo extraFunds outfile) -> do
          votingFunds <-
            runQuery db $ Query.queryVotingFunds networkId slotNo threshold

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
        
          BLC.writeFile outfile . toJSON $ genesis

    -- Voter Registration
    Register regOpts -> do
      eCfg <- runExceptT (Register.mkConfig regOpts)
      case eCfg of
        Left (err :: Register.ConfigError) ->
          fail $ show err
        Right (Register.Config addr voteSign paySign votePub networkId ttl outFile) -> do
          eResult <- runExceptT $ do
            SocketPath sockPath <-  readEnvSocketPath
            withlocalNodeConnectInfo (CardanoProtocol $ EpochSlots 21600) networkId sockPath $ \connectInfo -> do
              -- Create a vote registration, encoding our registration
              -- as transaction metadata. The transaction sends some
              -- unspent ADA back to us (minus a fee).
    
              -- Generate vote payload (vote information is encoded as metadata).
              let vote = createVote voteSign votePub
    
              -- Encode the vote as a transaction and sign it
              voteTx <- signTx paySign <$> encodeVote connectInfo ShelleyBasedEraShelley addr ttl vote
    
              -- Output helpful information
              liftIO . putStrLn $ "Vote public key used        (hex): " <> BSC.unpack (serialiseToRawBytesHex votePub)
              liftIO . putStrLn $ "Stake public key used       (hex): " <> BSC.unpack (verificationKeyRawBytes voteSign)
              liftIO . putStrLn $ "Vote registration signature (hex): " <> BSC.unpack (Base16.encode . Crypto.rawSerialiseSigDSIGN $ voteSignature vote)
    
              -- Output our vote transaction
              liftIO . writeFile outFile $ prettyTx voteTx
          case eResult of
            Left  (err :: AppError) -> fail $ show err
            Right ()                -> pure ()
      
toJSON :: Aeson.ToJSON a => a -> BLC.ByteString
toJSON = Aeson.encodePretty' (Aeson.defConfig { Aeson.confCompare = Aeson.compare, Aeson.confNumFormat = Aeson.Decimal })

runQuery :: DatabaseConfig -> ExceptT MetadataError (SqlPersistT IO) a -> IO a
runQuery dbConfig q = runNoLoggingT $ do
  logInfoN $ T.pack $ "Connecting to database at " <> _dbHost dbConfig
  withPostgresqlConn (pgConnectionString dbConfig) $ \backend -> do
    result <- Query.runQuery backend $ runExceptT $ q

    case result of
      Left (err :: MetadataError) -> fail $ show err
      Right x                     -> pure x
