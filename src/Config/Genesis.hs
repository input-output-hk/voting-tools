{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Handles configuration, which involves parsing command line
-- arguments and reading key files.

module Config.Genesis (Config(Config), opts, mkConfig, Opts(Opts), parseOpts, ConfigError) where

import           Control.Lens.TH (makeClassyPrisms)
import           Control.Monad.Except (ExceptT)

import           Options.Applicative

import           Cardano.Api (NetworkId, SlotNo)

import           Cardano.API.Extended (pNetworkId)
import           Cardano.CLI.Fetching (Threshold)
import           Cardano.CLI.Voting.Error ()

import           Config.Common (DatabaseConfig (DatabaseConfig), defaultThreshold, pSlotNo)

data Config = Config
    { cfgNetworkId         :: NetworkId
    -- ^ Network (mainnet / testnet magic)
    , cfgThreshold         :: Threshold
    -- ^ Minimum threshold of funds required to vote
    , cfgScale             :: Int
    -- ^ Scale the voting funds by this amount to arrive at the voting power
    , cfgDb                :: DatabaseConfig
    -- ^ cardano-db-sync database configuration
    , cfgSlot              :: Maybe SlotNo
    -- ^ Slot to view state of, defaults to tip of chain. Queries registrations placed before or equal to (<=) this slotNo.
    , cfgOutFile           :: FilePath
    -- ^ File to output genesis to
    }
  deriving (Eq, Show)

data ConfigError = ConfigFileJSONDecodeError FilePath String
    deriving (Show)

makeClassyPrisms ''ConfigError

mkConfig
  :: Opts
  -> ExceptT ConfigError IO Config
mkConfig (Opts networkId dbName dbUser dbHost mSlotNo threshold scale outfile) = do
  pure $ Config networkId threshold scale (DatabaseConfig dbName dbUser dbHost) mSlotNo outfile

data Opts = Opts
    { optNetworkId      :: NetworkId
    , optDbName         :: String
    , optDbUser         :: String
    , optDbHost         :: FilePath
    , optSlotNo         :: Maybe SlotNo
    , optThreshold      :: Threshold
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
  <*> optional pSlotNo
  <*> fmap fromIntegral (option auto (long "threshold" <> metavar "INT64" <> showDefault <> value defaultThreshold <> help "Minimum threshold of funds required to vote (Lovelace)"))
  <*> fmap fromIntegral (option auto (long "scale" <> metavar "DOUBLE" <> showDefault <> value (1 :: Integer) <> help "Scale the voting funds by this amount to arrive at the voting power"))
  <*> strOption (long "out-file" <> metavar "FILE" <> help "File to output the signed transaction to")

opts :: ParserInfo Opts
opts =
  info
    ( parseOpts )
    ( fullDesc
    <> progDesc "Create a genesis file"
    <> header "rego-fetch - a tool to create a genesis file"
    )
