{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.Logger (logInfoN, runNoLoggingT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import           Database.Persist.Postgresql (ConnectionString, SqlPersistT, withPostgresqlConn)
import qualified Options.Applicative as Opt

import           Cardano.CLI.Fetching (votingPowerFromRegistrationInfo)
import           Cardano.CLI.Query (MetadataRetrievalError)
import qualified Cardano.CLI.Query as Query
import           Config
import qualified Config.Genesis as Genesis

main :: IO ()
main = do
  options <- Opt.execParser opts

  case options of
    -- Genesis
    Genesis gOpts -> do
      eCfg <- runExceptT (Genesis.mkConfig gOpts)
      case eCfg of
        Left (err :: Genesis.ConfigError) ->
          fail $ show err
        Right (Genesis.Config networkId threshold scale db slotNo outfile) -> do
          votingFunds <-
            runQuery db $ Query.queryVotingFunds networkId slotNo threshold

          let
            scaled = votingPowerFromRegistrationInfo scale <$> votingFunds

          BLC.writeFile outfile . toJSON Aeson.Generic $ scaled

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
