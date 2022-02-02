{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Genesis where

import           Control.Lens ((%~), (.~))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (_Object, key)
import qualified Data.HashMap.Strict as HM
import           Data.Scientific (Scientific)
import           Data.Time (TimeOfDay (TimeOfDay), UTCTime (UTCTime), getCurrentTime,
                   timeOfDayToTime, timeToTimeOfDay)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import           Cardano.CLI.Fetching (Fund)

getBlockZeroDate :: IO UTCTime
getBlockZeroDate = do
  (UTCTime day dayTime) <- getCurrentTime
  let
    (TimeOfDay hr _ _ ) = timeToTimeOfDay dayTime
    blockZeroDate       = UTCTime day (timeOfDayToTime $ TimeOfDay hr 0 0)
  pure blockZeroDate

setInitialFunds :: [Fund] -> Aeson.Value -> Aeson.Value
setInitialFunds funds = _Object %~ HM.insert "initial" (Aeson.toJSON funds)

setBlockZeroDate :: UTCTime -> Aeson.Value -> Aeson.Value
setBlockZeroDate time =
  (key "blockchain_configuration"
    %~ (key "block0_date"
      .~ (Aeson.Number
         . (fromIntegral :: Integer -> Scientific)
         . floor
         . utcTimeToPOSIXSeconds
         $ time )))

unsafeDecodeJSONFile :: FilePath -> IO (Aeson.Value)
unsafeDecodeJSONFile fp = do
  result <- Aeson.eitherDecodeFileStrict' fp
  case result of
    Left err                               -> error err
    Right (genesisTemplate :: Aeson.Value) -> pure genesisTemplate
