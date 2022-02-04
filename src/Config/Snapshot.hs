{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Handles configuration, which involves parsing command line
-- arguments and reading key files.

module Config.Snapshot (Config(Config), opts, mkConfig, Opts(Opts), parseOpts, ConfigError) where

import           Control.Lens.TH (makeClassyPrisms)
import           Control.Monad.Except (ExceptT)

import           Options.Applicative

import           Cardano.Api (NetworkId, SlotNo (..))

import           Cardano.API.Extended (pNetworkId)
import           Cardano.CLI.Voting.Error ()

import           Config.Common (DatabaseConfig (DatabaseConfig), SlotInterval (..))

data Config = Config
    { cfgNetworkId         :: NetworkId
    -- ^ Network (mainnet / testnet magic)
    , cfgScale             :: Int
    -- ^ Scale the voting funds by this amount to arrive at the voting power
    , cfgDb                :: DatabaseConfig
    -- ^ cardano-db-sync database configuration
    , cfgSlotInterval      :: SlotInterval
    -- ^ Queries registrations made within this slot interval
    , cfgOutFile           :: FilePath
    -- ^ File to output snapshot to
    }
  deriving (Eq, Show)

data ConfigError = ConfigFileJSONDecodeError FilePath String
    deriving (Show)

makeClassyPrisms ''ConfigError

mkConfig
  :: Opts
  -> ExceptT ConfigError IO Config
mkConfig (Opts networkId dbName dbUser dbHost mSlotNo scale outfile) = do
  pure $ Config networkId scale (DatabaseConfig dbName dbUser dbHost) mSlotNo outfile

data Opts = Opts
    { optNetworkId      :: NetworkId
    , optDbName         :: String
    , optDbUser         :: String
    , optDbHost         :: FilePath
    , optSlotInterval   :: SlotInterval
    , optScale          :: Int
    , optOutFile        :: FilePath
    }
    deriving (Eq, Show)

parseOpts :: Parser Opts
parseOpts = Opts
  <$> pNetworkId
  <*> strOption (long "db" <> metavar "DB_NAME" <> showDefault <> value "cexplorer" <> help "Name of the cardano-db-sync database")
  <*> strOption (long "db-user" <> metavar "DB_USER" <> showDefault <> value "cexplorer" <> help "User to connect to the cardano-db-sync database with")
  <*> strOption (long "db-host" <> metavar "DB_HOST" <> showDefault <> value "/run/postgresql" <> help "Host for the cardano-db-sync database connection")
  <*> pSlotInterval
  <*> fmap fromIntegral (option auto (long "scale" <> metavar "DOUBLE" <> showDefault <> value (1 :: Integer) <> help "Scale the voting funds by this amount to arrive at the voting power"))
  <*> strOption (long "out-file" <> metavar "FILE" <> help "File to output the signed transaction to")

pSlotInterval :: Parser SlotInterval
pSlotInterval = SlotInterval
  <$> optional (option (SlotNo <$> auto)
          ( long "lower-slot-no"
          <> metavar "WORD64"
          <> help "Lower bound of slot number interval (inclusive). Registrations made on or after this slot number are included in the snapshot (default: -INF)"
          ))
  <*> optional (option (SlotNo <$> auto)
          ( long "upper-slot-no"
          <> metavar "WORD64"
          <> help "Upper bound of slot number interval (inclusive). Registrations made on or before this slot number are included in the snapshot (default: +INF)"
          ))

opts :: ParserInfo Opts
opts =
  info
    ( parseOpts <**> helper )
    ( fullDesc
    <> progDesc "Create a voting power snapshot"
    <> header "voting-tools snapshot - tool to grab snapshot of voting power"
    )
