{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Cardano.Catalyst.VotePower (getVoteRegistrationADA)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.Logger (LogLevel (..), LoggingT, filterLogger, logInfoN,
                   runStdoutLoggingT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import           Database.Persist.Postgresql (IsolationLevel (Serializable), SqlPersistT,
                   runSqlConnWithIsolation, withPostgresqlConn)
import qualified Options.Applicative as Opt

import qualified Cardano.Catalyst.Query.Sql as Sql
import           Config.Common (DatabaseConfig (..), pgConnectionString)
import qualified Config.Snapshot as Snapshot

main :: IO ()
main = do
  options <- Opt.execParser Snapshot.opts

  eCfg <- runExceptT (Snapshot.mkConfig options)
  case eCfg of
    Left (err :: Snapshot.ConfigError) ->
      fail $ show err
    Right (Snapshot.Config networkId _scale db slotNo outfile verbose) -> do
      votingPower <-
        runQuery db verbose $ getVoteRegistrationADA (Sql.sqlQuery) networkId slotNo

      BLC.writeFile outfile . toJSON Aeson.Generic $ votingPower

toJSON :: Aeson.ToJSON a => Aeson.NumberFormat -> a -> BLC.ByteString
toJSON numFormat = Aeson.encodePretty' (Aeson.defConfig { Aeson.confCompare = Aeson.compare, Aeson.confNumFormat = numFormat })

runQuery :: DatabaseConfig -> Bool -> SqlPersistT (LoggingT IO) a -> IO a
runQuery dbConfig verbose q = runStdoutLoggingT $ filterLogger f $ do
    logInfoN $ T.pack $ "Connecting to database at " <> _dbHost dbConfig
    withPostgresqlConn (pgConnectionString dbConfig) $ \backend -> do
      runSqlConnWithIsolation q backend Serializable
  where
    f _ level
      | verbose = True
      | otherwise = level > LevelDebug
