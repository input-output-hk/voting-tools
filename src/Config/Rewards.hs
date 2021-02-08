module Config.Rewards (Config(Config), opts, mkConfig, Opts(Opts), parseOpts) where

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

import           Cardano.Api (Address, Bech32DecodeError, FileError, Lovelace, NetworkId,
                     PaymentKey, SigningKey, StakeKey, Witness)
import qualified Cardano.Api as Api
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
    , cfgDb                :: DatabaseConfig
    -- ^ cardano-db-sync database configuration
    , cfgSlot              :: Maybe SlotNo
    -- ^ Slot to view state of, defaults to tip of chain
    , cfgTotalRewards      :: Lovelace
    -- ^ Total rewards to distribute between voters
    , cfgOutFile           :: FilePath
    -- ^ File to write rewards info to
    }
  deriving (Eq, Show)

mkConfig
  :: Opts
  -> Config
mkConfig (Opts networkId dbName dbUser dbHost mSlotNo threshold totalRewards outfile) = do
  Config networkId threshold (DatabaseConfig dbName dbUser dbHost) mSlotNo totalRewards outfile

data Opts = Opts
    { optNetworkId      :: NetworkId
    , optDbName         :: String
    , optDbUser         :: String
    , optDbHost         :: FilePath
    , optSlotNo         :: Maybe SlotNo
    , optThreshold      :: Threshold
    , optTotalRewards   :: Lovelace
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
  <*> fmap fromIntegral (option auto (long "total-rewards" <> metavar "INT64" <> help "Total rewards to distribute between voters"))
  <*> strOption (long "out-file" <> metavar "FILE" <> help "File to output the signed transaction to")

opts =
  info
    ( parseOpts )
    ( fullDesc
    <> progDesc "Generate a file describing how rewards should be distributed between voters"
    <> header "rewards - a tool for distributing rewards"
    )
