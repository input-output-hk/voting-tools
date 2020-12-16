{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Handles configuration, which involves parsing command line
-- arguments and reading key files.

module Config (Config(Config), opts, mkConfig) where

import           Control.Exception.Safe (try)
import           Control.Lens (( # ))
import           Control.Lens.TH
import           Control.Monad.Except (ExceptT, MonadError, catchError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           Options.Applicative

import           Cardano.API (Address, Bech32DecodeError, FileError, NetworkId, PaymentKey,
                     SigningKey, StakeKey, Witness, Lovelace(Lovelace))
import qualified Cardano.API as Api
import           Cardano.Api.TextView (TextViewError)
import           Cardano.Api.Typed (Shelley, SlotNo (SlotNo), TTL)
import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath)
import           Cardano.CLI.Shelley.Commands (WitnessFile (WitnessFile))
import           Cardano.CLI.Shelley.Key (InputDecodeError)
import           Cardano.CLI.Types (SigningKeyFile (..), SocketPath)

import           Cardano.API.Extended (AsBech32DecodeError (_Bech32DecodeError),
                     AsFileError (_FileIOError, __FileError),
                     AsInputDecodeError (_InputDecodeError), AsType (AsVotingKeyPublic),
                     VotingKeyPublic, pNetworkId, parseAddress,
                     readSigningKeyFile, readerFromAttoParser)
import           Cardano.CLI.Voting.Error (AsTextViewError (_TextViewError))

data DatabaseConfig
  = DatabaseConfig { _dbName       :: String
                   , _dbUser       :: String
                   , _dbSocketPath :: FilePath
                   }
  deriving (Eq, Show)

data Config = Config
    { cfgNetworkId         :: NetworkId
    -- ^ Network (mainnet / testnet magic)
    , cfgThreshold         :: Threshold
    -- ^ Minimum threshold of funds required to vote
    , cfgDb                :: DatabaseConfig
    -- ^ cardano-db-sync database configuration
    , cfgSlot              :: Maybe SlotNo
    -- ^ Slot to view state of, defaults to tip of chain
    , cfgExtraFunds        :: VotingFunds
    -- ^ Extra funds to consider in the threshold test
    }
  deriving (Eq, Show)

data ConfigError = ConfigFileJSONDecodeError FilePath String
    deriving (Show)

makeClassyPrisms ''ConfigError

mkConfig
  :: Opts
  -> ExceptT ConfigError IO Config
mkConfig (Opts networkId dbName dbUser dbSocket mExtraFundsFp mSlotNo threshold) = do
  extraFunds <-
    case mExtraFundsFp of
      Nothing             -> pure mempty
      (Just extraFundsFp) -> readExtraFundsFile extraFundsFp

  pure $ Config networkId threshold (DatabaseConfig dbName dbUser dbSocket) mSlotNo extraFunds

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
    , optDbSocket       :: FilePath
    , optExtraFundsFile :: Maybe FilePath
    , optSlotNo         :: Maybe SlotNo
    , optThreshold      :: Threshold
    }
    deriving (Eq, Show)

parseOpts :: Parser Opts
parseOpts = Opts
  <$> pNetworkId
  <*> strOption (long "db" <> metavar "DB_NAME" <> showDefault <> value "cexplorer" <> help "Name of the cardano-db-sync database")
  <*> strOption (long "db-user" <> metavar "DB_USER" <> showDefault <> value "cexplorer" <> help "User to connect to the cardano-db-sync database with")
  <*> strOption (long "db-sock" <> metavar "DB_SOCK" <> showDefault <> value "/run/postgresql" <> help "socket file for the cardano-db-sync database connection")
  <*> option auto (long "extra-funds" <> metavar "FILE" <> help "File containing extra funds to include in the query (JSON)")
  <*> pSlotNo
  <*> option auto (long "threshold" <> metavar "INT64" <> help "Minimum threshold of funds required to vote (Lovelace)")

opts =
  info
    ( parseOpts <**> helper )
    ( fullDesc
    <> progDesc "Fetch valid voter registrations"
    <> header "rego-fetch - a tool to fetch voter registrations"
    )

pSlotNo :: Parser (Maybe SlotNo)
pSlotNo = (fmap SlotNo)
    <$> option auto
          ( long "slot-no"
          <> metavar "WORD64"
          <> help "Slot number to query"
          )
