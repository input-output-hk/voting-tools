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

import           Options.Applicative

import           Cardano.Api (NetworkId, SlotNo)

import           Cardano.API.Extended (pNetworkId)

import           Config.Common (DatabaseConfig (DatabaseConfig), pSlotNo)

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
    }
  deriving (Eq, Show)

data ConfigError = ConfigFileJSONDecodeError FilePath String
    deriving (Show)

makeClassyPrisms ''ConfigError

mkConfig
  :: Opts
  -> ExceptT ConfigError IO Config
mkConfig (Opts networkId dbName dbUser dbHost dbPass mSlotNo scale outfile) = do
  pure $ Config networkId scale (DatabaseConfig dbName dbUser dbHost dbPass) mSlotNo outfile

data Opts = Opts
    { optNetworkId      :: NetworkId
    , optDbName         :: String
    , optDbUser         :: String
    , optDbHost         :: FilePath
    , optDbPass         :: Maybe String
    , optSlotNo         :: Maybe SlotNo
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
  <*> optional (strOption (long "db-pass" <> metavar "DB_PASS" <> showDefault <> value "" <> help "Password for the cardano-db-sync database connection"))
  <*> optional pSlotNo
  <*> fmap fromIntegral (option auto (long "scale" <> metavar "DOUBLE" <> showDefault <> value (1 :: Integer) <> help "Scale the voting funds by this amount to arrive at the voting power"))
  <*> strOption (long "out-file" <> metavar "FILE" <> help "File to output the signed transaction to")

opts :: ParserInfo Opts
opts =
  info
    ( parseOpts <**> helper )
    ( fullDesc
    <> progDesc "Create a voting power snapshot"
    <> header "voting-tools snapshot - tool to grab snapshot of voting power"
    )
