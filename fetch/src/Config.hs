{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
                     SigningKey, StakeKey, Witness)
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
                     VotingKeyPublic, deserialiseFromBech32, pNetworkId, parseAddress,
                     readSigningKeyFile, readerFromAttoParser)
import           Cardano.CLI.Voting.Error (AsTextViewError (_TextViewError))

type Threshold = Int

data VotingFunds
  = VotingFunds { _votingFunds :: Map VotingKeyPublic Lovelace }

instance FromJSON VotingFunds where
    parseJSON = withObject "VotingFunds" $ \v ->
      let
        decodeVotingKeyPublic = withText "VotingKeyPublic" $ \t ->
          case Base16.decode t of
            Left err    -> fail $ "key " <> show t <> " could not be interpreted as hexadecimal."
            Right bytes ->
              case deserialiseFromRawBytes bytes of
                Nothing         -> fail $ "failed to deserialise VotingKeyPublic from raw bytes: " <> show bytes <> "."
                Right votingKey -> pure votingKey
  
        decodeLovelace = withNumber "Lovelace" $ \n ->
          case toBoundedInteger n of
            Nothing  -> fail $ "failed to convert " <> show n <> " into a valid integer."
            Just num -> pure $ Lovelace num
  
        kvs = HM.toList v

      kvs' <- traverse ((k, v) -> (,) <$> decodeVotingKeyPublic k <*> decodeLovelace v) kvs
      VotingFunds $ M.fromList kvs

instance ToJSON VotingFunds where
  toJSON (VotingFunds map) =
    let 
      kvs = M.toList map

      toJSONVotingKeyPublic = T.pack . BC.unpack . Base16.encode . serialiseAsRawBytes
      toJSONLovelace (Lovelace n) = Aeson.Number $ fromInteger n

      kvs' = (\(k,v) -> (toJSONVotingKeyPublic k, toJSONLovelace v)) <$> kvs
    in
      Aeson.Object $ HM.fromList kvs'

data DatabaseConfig
  = DatabaseConfig { _dbName       :: String
                   , _dbUser       :: String
                   , _dbSocketPath :: FilePath
                   }

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
    deriving (Show)

data ConfigError = ConfigFileJSONDecodeError FilePath String
    deriving (Show)

makePrisms ''ConfigError

mkConfig
  :: Opts
  -> ExceptT ConfigError IO Config
mkConfig (Opts networkId dbName dbUser dbSocket mExtraFundsFp mSlotNo threshold) = do
  extraFunds <-
    case mExtraFundsFp of
      Nothing             -> mempty
      (Just extraFundsfp) -> readExtraFundsFile extraFundsFp

  pure $ Config networkId threshold (DatabaseConfig dbName dbUser dbSocket) mSlotNo extraFunds

readExtraFundsFile
  :: ( MonadIO m
     , MonadError e m
     , AsConfigError e
     )
  => FilePath -> m FundsMap
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
  <*> strOption (long "database-name" <> short "db" <> metavar "DB_NAME" <> showDefault <> value "cexplorer" <> help "Name of the cardano-db-sync database")
  <*> strOption (long "database-user" <> short "db-user" <> metavar "DB_USER" <> showDefault <> value "cexplorer" <> help "User to connect to the cardano-db-sync database with")
  <*> strOption (long "database-socket-file" <> short "db-sock" <> metavar "DB_SOCK" <> showDefault <> value "/run/postgresql" <> help "socket file for the cardano-db-sync database connection")
  <*> strOption (long "extra-funds" <> metavar "FILE" <> help "File containing extra funds to include in the query (JSON)")
  <*> pSlotNo
  <*> auto (long "threshold" <> metavar "WORD64" <> help "Minimum threshold of funds required to vote")

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
