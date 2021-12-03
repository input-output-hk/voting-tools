module Config.Rewards (Config(Config), opts, mkConfig, Opts(Opts), parseOpts) where

import           Options.Applicative

import           Cardano.Api (Lovelace, NetworkId, SlotNo)

import           Cardano.API.Extended (pNetworkId)
import           Cardano.CLI.Fetching (Threshold)
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
  <*> fmap (fromIntegral :: Integer -> Lovelace) (option auto (long "total-rewards" <> metavar "INT64" <> help "Total rewards to distribute between voters"))
  <*> strOption (long "out-file" <> metavar "FILE" <> help "File to output the signed transaction to")

opts :: ParserInfo Opts
opts =
  info
    ( parseOpts )
    ( fullDesc
    <> progDesc "Generate a file describing how rewards should be distributed between voters"
    <> header "rewards - a tool for distributing rewards"
    )
