{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Cardano.Catalyst.Registration.Types.Purpose
Description : Purpose for vote registrations
Maintainer  : sevanspowell
Stability   : experimental

This module encapsulates the idea of a 'vote registration purpose'.

A vote registration purpose can be any non-negative integer.

At this stage only a single vote registration purpose exists: "Catalyst". The
"Catalyst" vote purpose is represented by the integer 0.

-}

module Cardano.Catalyst.Registration.Types.Purpose
  ( Purpose
  , catalystPurpose
  , mkPurpose
  , purposeNumber
  , toTxMetadataValue
  , fromTxMetadataValue
  )
  where

import           Data.Aeson (FromJSON (..), ToJSON (..), withScientific)
import           Data.Scientific (floatingOrInteger)
import           Data.Text (Text)

import qualified Cardano.Api as Api
import qualified Data.Text as T

-- | A vote registration purpose.
data Purpose = CatalystPurpose
             -- ^ A vote registration for the Catalyst project.
             | OtherPurpose Integer
             -- ^ Other vote registration purpose.
  deriving (Eq, Ord, Show)

-- | Creates a vote registration purpose from an integer.
--
-- Will throw an error if provided with a negative integer.
mkPurpose :: Integer -> Either Text Purpose
mkPurpose 0         = Right CatalystPurpose
mkPurpose x | x > 0 = Right $ OtherPurpose x
mkPurpose _         = Left "expected a positive integer, got a negative integer"

-- | Retrieves the non-negative integer representing a vote registration
--   purpose.
purposeNumber :: Purpose -> Integer
purposeNumber CatalystPurpose  = 0
purposeNumber (OtherPurpose x) = x

-- | The Catalyst vote registration purpose.
catalystPurpose :: Purpose
catalystPurpose = CatalystPurpose

-- | Convert a vote registration purpose to cardano-api transaction metadata.
toTxMetadataValue :: Purpose -> Api.TxMetadataValue
toTxMetadataValue CatalystPurpose  = Api.TxMetaNumber 0
toTxMetadataValue (OtherPurpose x) = Api.TxMetaNumber x

-- | Parse a vote registration purpose from cardano-api transaction metadata.
fromTxMetadataValue :: Api.TxMetadataValue -> Either Text Purpose
fromTxMetadataValue (Api.TxMetaNumber x) =
  mkPurpose x
fromTxMetadataValue x                =
  Left $ "expected a number, got: " <> T.pack (show x)

instance ToJSON Purpose where
  toJSON = Api.metadataValueToJsonNoSchema . toTxMetadataValue

instance FromJSON Purpose where
  parseJSON = withScientific "voting purpose" $ \s ->
    case floatingOrInteger s of
      Left (f :: Double) ->
        fail $
          "Expected non-negative integer, found floating number: " <> show f
      Right i ->
        either (fail . T.unpack) pure $ mkPurpose i
