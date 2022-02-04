{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Config.Common where

import           Cardano.Api (AnyCardanoEra (..), CardanoEra (..), SlotNo (..))
import           Data.Foldable (asum)
import           Data.Text (Text)
import           Options.Applicative (Parser, auto, flag', help, long, metavar, option)

import qualified Data.Text as T

data DatabaseConfig
  = DatabaseConfig { _dbName       :: String
                   , _dbUser       :: String
                   , _dbHost       :: String
                   }
  deriving (Eq, Show)

data SlotInterval
  = SlotInterval { _slotNoLowerIntervalInclusive :: Maybe SlotNo
                 , _slotNoUpperIntervalInclusive :: Maybe SlotNo
                 }
  deriving (Eq, Show)

inclusiveUpperInterval :: SlotInterval -> Maybe SlotNo
inclusiveUpperInterval = _slotNoUpperIntervalInclusive

inclusiveLowerInterval :: SlotInterval -> Maybe SlotNo
inclusiveLowerInterval = _slotNoUpperIntervalInclusive

slotIntervalQuery :: SlotInterval -> Text
slotIntervalQuery (SlotInterval Nothing Nothing)
  = ""
slotIntervalQuery (SlotInterval (Just lower) Nothing)
  = "block.slot_no >= " <> T.pack (show $ unSlotNo lower)
slotIntervalQuery (SlotInterval Nothing (Just upper))
  = "block.slot_no <= " <> T.pack (show $ unSlotNo upper)
slotIntervalQuery (SlotInterval lower upper)
  = slotIntervalQuery (SlotInterval lower Nothing)
  <> " AND "
  <> slotIntervalQuery (SlotInterval Nothing upper)

isInfiniteRange :: SlotInterval -> Bool
isInfiniteRange = (SlotInterval Nothing Nothing ==)

pSlotNo :: Parser SlotNo
pSlotNo = SlotNo
    <$> option auto
          ( long "slot-no"
          <> metavar "WORD64"
          <> help "Slot number to query"
          )

pCardanoEra :: Parser AnyCardanoEra
pCardanoEra = asum
  [ flag' (AnyCardanoEra ByronEra)
      (  long "byron-era"
      <> help "Specify the Byron era"
      )
  , flag' (AnyCardanoEra ShelleyEra)
      (  long "shelley-era"
      <> help "Specify the Shelley era"
      )
  , flag' (AnyCardanoEra AllegraEra)
      (  long "allegra-era"
      <> help "Specify the Allegra era"
      )
  , flag' (AnyCardanoEra MaryEra)
      (  long "mary-era"
      <> help "Specify the Mary era (default)"
      )
  , flag' (AnyCardanoEra AlonzoEra)
      (  long "alonzo-era"
      <> help "Specify the Alonzo era"
      )

    -- Default for now:
  , pure (AnyCardanoEra MaryEra)
  ]
