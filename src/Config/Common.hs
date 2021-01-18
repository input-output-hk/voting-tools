module Config.Common where

import           Options.Applicative (Parser, auto, help, long, metavar, option)

import           Cardano.Api.Typed (SlotNo (SlotNo))

data DatabaseConfig
  = DatabaseConfig { _dbName       :: String
                   , _dbUser       :: String
                   , _dbHost       :: String
                   }
  deriving (Eq, Show)

pSlotNo :: Parser SlotNo
pSlotNo = SlotNo
    <$> option auto
          ( long "slot-no"
          <> metavar "WORD64"
          <> help "Slot number to query"
          )

defaultThreshold :: Integer
defaultThreshold = 8000000000
