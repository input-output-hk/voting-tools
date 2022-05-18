{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snapshot where

import           Control.Lens
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens
import qualified Data.HashMap.Strict as HM
import           Data.List (sortOn)
import           Data.Scientific (Scientific)
import           Data.Time (TimeOfDay (TimeOfDay), UTCTime (UTCTime), getCurrentTime,
                   timeOfDayToTime, timeToTimeOfDay)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Vector as Vector

import           Cardano.API.Extended
import           Cardano.API.Jormungandr
import           Cardano.Api

getBlockZeroDate :: IO UTCTime
getBlockZeroDate = do
  (UTCTime day dayTime) <- getCurrentTime
  let
    (TimeOfDay hr _ _ ) = timeToTimeOfDay dayTime
    blockZeroDate       = UTCTime day (timeOfDayToTime $ TimeOfDay hr 0 0)
  pure blockZeroDate

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
    Right (snapshotTemplate :: Aeson.Value) -> pure snapshotTemplate

getAllFundContents :: Aeson.Value -> Aeson.Value
getAllFundContents x =
  x & (^.. key "initial" . values . key "fund". _Array)
    & mconcat
    & Vector.toList
    & sortOn (^?! _Object . at "address")
    & Vector.fromList
    & Aeson.Array

getSnapshotContents :: NetworkId -> Aeson.Value -> Aeson.Value
getSnapshotContents networkId x =
  x & values . _Object %~ sans "stake_public_key" . sans "reward_address"
    & values . key "voting_public_key" . _JSON %~ (\(votePub :: VotingKeyPublic) -> addressFromVotingKeyPublic networkId votePub)
    & (^.. values . _Object)
    & foldr (\m acc ->
                 let
                   votePub = m ^?! at "voting_public_key" . _Just . _String
                   votePower = m ^?! at "voting_power" . _Just . _Number
                 in
                   HM.insertWith (+) votePub votePower acc
              ) mempty
    & HM.toList
    & sortOn fst
    & foldMap (\(votePub, votePower) -> [ Aeson.Object $ HM.fromList
        [ ("address", Aeson.String votePub)
        , ("value", Aeson.Number votePower)]
        ]
              )
    & Vector.fromList
    & Aeson.Array

-- usageEg = do
--     old <- unsafeDecodeJSONFile "cmp-old.json"
--     new <- unsafeDecodeJSONFile "cmp-new.json"

--     let
--       encodeFilePretty fp =  BSL.writeFile fp . encodePretty

--     encodeFilePretty "cmp-old-diff.json" $ getAllFundContents old
--     encodeFilePretty "cmp-new-diff.json" $ getSnapshotContents Mainnet new
