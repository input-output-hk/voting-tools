{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Handles configuration, which involves parsing command line
-- arguments and reading key files.

module Config.Genesis (Config(Config), opts, mkConfig, Opts(Opts), parseOpts, ConfigError) where

import           Control.Exception.Safe (try)
import           Control.Lens (( # ))
import           Control.Lens.TH
import           Control.Monad.Except (ExceptT, MonadError, catchError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BC
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           Options.Applicative

import           Cardano.API (Address, Bech32DecodeError, FileError, Lovelace, NetworkId,
                     PaymentKey, SigningKey, StakeKey, Witness)
import qualified Cardano.API as Api
import           Cardano.Api.Typed (Shelley, SlotNo (SlotNo), TextEnvelopeError)
import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath)
import           Cardano.CLI.Shelley.Commands (WitnessFile (WitnessFile))
import           Cardano.CLI.Shelley.Key (InputDecodeError)
import           Cardano.CLI.Types (SigningKeyFile (..), SocketPath)

import           Cardano.API.Extended (AsBech32DecodeError (_Bech32DecodeError),
                     AsFileError (_FileIOError, __FileError),
                     AsInputDecodeError (_InputDecodeError), AsType (AsVotingKeyPublic),
                     VotingKeyPublic, pNetworkId, readSigningKeyFile, readerFromAttoParser)
import           Cardano.CLI.Fetching (Threshold, VotingFunds)
import           Cardano.CLI.Voting.Error ()

import           Config.Common (DatabaseConfig (DatabaseConfig), defaultThreshold, pSlotNo)

data Config = Config
    { cfgNetworkId         :: NetworkId
    -- ^ Network (mainnet / testnet magic)
    , cfgThreshold         :: Threshold
    -- ^ Minimum threshold of funds required to vote
    , cfgScale             :: Double
    -- ^ Scale the voting funds by this amount to arrive at the voting power
    , cfgDb                :: DatabaseConfig
    -- ^ cardano-db-sync database configuration
    , cfgSlot              :: Maybe SlotNo
    -- ^ Slot to view state of, defaults to tip of chain
    , cfgExtraFunds        :: VotingFunds
    -- ^ Extra funds to consider in the threshold test
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
mkConfig (Opts networkId dbName dbUser dbHost mExtraFundsFp mSlotNo threshold scale outfile) = do
  extraFunds <-
    case mExtraFundsFp of
      Nothing             -> pure mempty
      (Just extraFundsFp) -> readExtraFundsFile extraFundsFp

  pure $ Config networkId threshold scale (DatabaseConfig dbName dbUser dbHost) mSlotNo extraFunds outfile

readExtraFundsFile
  :: ( MonadIO m
     , MonadError e m
     , AsConfigError e
     )
  => FilePath -> m VotingFunds
readExtraFundsFile fp = do
  result <- liftIO . Aeson.eitherDecodeFileStrict' $ fp
  either (\e -> throwError . (_ConfigFileJSONDecodeError #) $ (fp, e)) pure result

data Opts = Opts
    { optNetworkId      :: NetworkId
    , optDbName         :: String
    , optDbUser         :: String
    , optDbHost         :: FilePath
    , optExtraFundsFile :: Maybe FilePath
    , optSlotNo         :: Maybe SlotNo
    , optThreshold      :: Threshold
    , optScale          :: Double
    , optOutFile        :: FilePath
    }
    deriving (Eq, Show)

parseOpts :: Parser Opts
parseOpts = Opts
  <$> pNetworkId
  <*> strOption (long "db" <> metavar "DB_NAME" <> showDefault <> value "cexplorer" <> help "Name of the cardano-db-sync database")
  <*> strOption (long "db-user" <> metavar "DB_USER" <> showDefault <> value "cexplorer" <> help "User to connect to the cardano-db-sync database with")
  <*> strOption (long "db-host" <> metavar "DB_HOST" <> showDefault <> value "/run/postgresql" <> help "Host for the cardano-db-sync database connection")
  <*> optional (strOption (long "extra-funds" <> metavar "FILE" <> help "File containing extra funds to include in the query (JSON)"))
  <*> optional pSlotNo
  <*> fmap fromIntegral (option auto (long "threshold" <> metavar "INT64" <> showDefault <> value defaultThreshold <> help "Minimum threshold of funds required to vote (Lovelace)"))
  <*> fmap fromIntegral (option auto (long "scale" <> metavar "DOUBLE" <> showDefault <> value 1 <> help "Scale the voting funds by this amount to arrive at the voting power"))
  <*> strOption (long "out-file" <> metavar "FILE" <> help "File to output the signed transaction to")

opts =
  info
    ( parseOpts )
    ( fullDesc
    <> progDesc "Create a genesis file"
    <> header "rego-fetch - a tool to create a genesis file"
    )
