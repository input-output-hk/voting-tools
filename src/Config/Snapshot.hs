{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Handles configuration, which involves parsing command line
-- arguments and reading key files.

module Config.Snapshot
  ( Config(Config)
  , opts
  , mkConfig
  , Opts(Opts)
  , parseOpts
  , ConfigError
  ) where

import           Control.Lens.TH (makeClassyPrisms)
import           Control.Monad.Except (ExceptT)
import           Options.Applicative (Parser, ParserInfo, auto, flag, fullDesc, header, help, helper,
                   info, long, metavar, option, optional, progDesc, showDefault, strOption, value,
                   (<**>))

import           Cardano.API.Extended (pNetworkId)
import           Cardano.Api (NetworkId, SlotNo)

import           Config.Common (DatabaseConfig (DatabaseConfig), pSlotNo, versionOption)

data Config = Config
    { cfgNetworkId         :: NetworkId
    -- ^ Network (mainnet / testnet magic)
    , cfgScale             :: Int
    -- ^ Scale the voting funds by this amount to arrive at the voting power
    , cfgDb                :: DatabaseConfig
    -- ^ cardano-db-sync database configuration
    , cfgSlot              :: Maybe SlotNo
    -- ^ Slot to view state of, defaults to tip of chain. Queries registrations placed before or equal to (<=) this slotNo.
    , cfgOutFile           :: FilePath
    -- ^ File to output snapshot to
    , cfgVerbose           :: Bool
    -- ^ Enable verbose logging
    }
  deriving (Eq, Show)

data ConfigError = ConfigFileJSONDecodeError FilePath String
    deriving (Show)

makeClassyPrisms ''ConfigError

mkConfig
  :: Opts
  -> ExceptT ConfigError IO Config
mkConfig (Opts networkId dbName dbUser dbHost dbPass mSlotNo scale outfile verbose) = do
  pure $ Config networkId scale (DatabaseConfig dbName dbUser dbHost dbPass) mSlotNo outfile verbose

data Opts = Opts
    { optNetworkId      :: NetworkId
    , optDbName         :: String
    , optDbUser         :: String
    , optDbHost         :: FilePath
    , optDbPass         :: Maybe String
    , optSlotNo         :: Maybe SlotNo
    , optScale          :: Int
    , optOutFile        :: FilePath
    , optVerbose        :: Bool
    }
    deriving (Eq, Show)

parseOpts :: Parser Opts
parseOpts = Opts
  <$> pNetworkId
  <*> strOption (long "db" <> metavar "DB_NAME" <> showDefault <> value "cexplorer" <> help "Name of the cardano-db-sync database")
  <*> strOption (long "db-user" <> metavar "DB_USER" <> showDefault <> value "cexplorer" <> help "User to connect to the cardano-db-sync database with")
  <*> strOption (long "db-host" <> metavar "DB_HOST" <> showDefault <> value "/run/postgresql" <> help "Host for the cardano-db-sync database connection")
  <*> optional (strOption (long "db-pass" <> metavar "DB_PASS" <> showDefault <> value "" <> help "Password for the cardano-db-sync database connection"))
  <*> optional pSlotNo
  <*> fmap fromIntegral (option auto (long "scale" <> metavar "DOUBLE" <> showDefault <> value (1 :: Integer) <> help "Scale the voting funds by this amount to arrive at the voting power"))
  <*> strOption (long "out-file" <> metavar "FILE" <> help "File to output the signed transaction to")
  <*> flag False True (long "verbose" <> help "Adds more verbose logs.")

opts :: ParserInfo Opts
opts =
  info
    ( parseOpts <**> versionOption "0.3.0.0" <**> helper )
    ( fullDesc
    <> progDesc "Create a voting power snapshot"
    <> header "voting-tools snapshot - tool to grab snapshot of voting power"
    )
