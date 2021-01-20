{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Config.Common where

import qualified Data.ByteString.Char8 as BC
import Data.Foldable (asum)
import Cardano.Api (AnyCardanoEra(..), IsShelleyBasedEra(shelleyBasedEra), CardanoEra(..))
import           Options.Applicative (Parser, auto, help, long, metavar, option, flag')

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

pCardanoEra :: Parser AnyCardanoEra
pCardanoEra = asum
  [ flag' (AnyCardanoEra ByronEra)
      (  long "byron-era"
      <> help "Specify the Byron era"
      )
  , flag' (AnyCardanoEra ShelleyEra)
      (  long "shelley-era"
      <> help "Specify the Shelley era (default)"
      )
  , flag' (AnyCardanoEra AllegraEra)
      (  long "allegra-era"
      <> help "Specify the Allegra era"
      )
  , flag' (AnyCardanoEra MaryEra)
      (  long "mary-era"
      <> help "Specify the Mary era"
      )

    -- Default for now:
  , pure (AnyCardanoEra ShelleyEra)
  ]
