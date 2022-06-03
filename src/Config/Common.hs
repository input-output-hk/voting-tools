{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Config.Common where

import           Cardano.Api (AnyCardanoEra (..), CardanoEra (..), SlotNo (..))
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable (asum)
import           Database.Persist.Postgresql (ConnectionString)
import           Options.Applicative (Parser, auto, flag', help, infoOption, long, metavar, option)

data DatabaseConfig
  = DatabaseConfig { _dbName       :: String
                   , _dbUser       :: String
                   , _dbHost       :: String
                   , _dbPassword   :: Maybe String
                   }
  deriving (Eq, Show)

pgConnectionString :: DatabaseConfig -> ConnectionString
pgConnectionString (DatabaseConfig dbName dbUser dbHost mDbPassword) =
  BSC.pack
    $ "host=" <> dbHost
    <> " dbname=" <> dbName
    <> " user=" <> dbUser
    <> maybe "" (" password=" <>) mDbPassword

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

versionOption :: String -> Parser (a -> a)
versionOption v =
  infoOption ("v" <> v) (long "version" <> help "Show version")
