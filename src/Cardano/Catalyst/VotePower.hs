{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |

Module – Cardano.Catalyst.VotePower
Description – Application to find voting power associated with vote registrations.
Maintainer – sevanspowell
Stability – experimental

This module represents the "application layer" of the voting-tools "find vote
power" executable. It provides a single function that provides the amount of ADA
associated with each of the latest vote registrations available at a particular
point in time.
-}

module Cardano.Catalyst.VotePower where

import           Cardano.API.Extended (VotingKeyPublic)
import           Cardano.Catalyst.Crypto (StakeVerificationKey)
import           Cardano.Catalyst.Query.Types (Query (..))
import           Cardano.Catalyst.Registration (Delegations (..), VoteRewardsAddress, Vote,
                   catalystPurpose, filterLatestRegistrations, parseRegistration, purposeNumber,
                   voteRegistrationDelegations, voteRegistrationPurpose,
                   voteRegistrationRewardsAddress, voteRegistrationStakeAddress,
                   voteRegistrationVerificationKey)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Char (toLower)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe (fromMaybe)
import           Data.Ratio (Ratio, (%))
import           GHC.Generics (Generic)

import qualified Cardano.Api as Api
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE

data VotingPower
  = VotingPower { _powerDelegations    :: Delegations VotingKeyPublic
                , _powerStakePublicKey :: StakeVerificationKey
                , _powerRewardsAddress :: VoteRewardsAddress
                , _powerVotingPower    :: Integer
                , _powerVotingPurpose  :: Integer
                }
  deriving (Eq, Ord, Show, Generic)

votingPowerJsonParserOptions :: Aeson.Options
votingPowerJsonParserOptions = Aeson.defaultOptions
    { Aeson.fieldLabelModifier = fmap toLower . Aeson.camelTo2 '_' . (drop 6) }

instance ToJSON VotingPower where
  toJSON = Aeson.genericToJSON votingPowerJsonParserOptions

instance FromJSON VotingPower where
  parseJSON = Aeson.genericParseJSON votingPowerJsonParserOptions

jsonParserOptions :: Aeson.Options
jsonParserOptions = Aeson.defaultOptions { Aeson.fieldLabelModifier = (fmap toLower) . (drop 2) }

getVoteRegistrationADA
  :: ( Monad m
     , Ord t
     )
  => Query m t
  -> Api.NetworkId
  -> Maybe Api.SlotNo
  -> m [VotingPower]
getVoteRegistrationADA q nw slotNo = do
  (regosRaw :: [(t, Aeson.Value)]) <- (queryVoteRegistrations q) slotNo

  let
    regos :: [(t, Vote)]
    regos =
      flip foldMap regosRaw $ \(t, regoRaw) -> do
        case parseRegistration regoRaw of
          Left _e    -> []
          Right rego -> [(t, rego)]

  let
    latestRegos :: [Vote]
    latestRegos = filterLatestRegistrations regos

  (regoStakes :: [(Api.StakeAddress, Integer)]) <-
    (queryStakeValues q) slotNo
      $ fmap (voteRegistrationStakeAddress nw) latestRegos

  let
    regoValues :: [(Vote, Integer)]
    regoValues = (zip latestRegos . fmap snd) regoStakes

  pure $ votingPowerFromRegoValues regoValues

votingPowerFromRegoValues :: [(Vote, Integer)] -> [VotingPower]
votingPowerFromRegoValues regoValues =
  fmap (uncurry votingPowerFromRegoValue) regoValues

votingPowerFromRegoValue :: Vote -> Integer -> VotingPower
votingPowerFromRegoValue rego power =
  let
    ds :: Delegations VotingKeyPublic
    ds = voteRegistrationDelegations rego

    stakeKey    = voteRegistrationVerificationKey rego
    rewardsAddr = voteRegistrationRewardsAddress rego
    purpose     =
      purposeNumber $ fromMaybe catalystPurpose $ voteRegistrationPurpose rego
  in
    VotingPower ds stakeKey rewardsAddr (max power 0) purpose

delegateVotingPower
  :: forall key
   . Delegations key
  -> Integer
  -> NonEmpty (key, Integer)
delegateVotingPower (LegacyDelegation key) power =
  (key, max 0 power) NE.:| []
delegateVotingPower (Delegations keyWeights)   power =
  let
    -- Get the total weight of all delegations.
    weightTotal :: Integer
    weightTotal = sum $ fmap (fromIntegral . snd) keyWeights

    -- Clamp power to 0 in case its negative.
    powerTotal = max power 0
  in
    let
      -- Divide each weight by the total to get the percentage weight of
      -- each delegation.
      percentage :: NonEmpty (key, Ratio Integer)
      percentage =
        -- Prevent divide by zero
        if weightTotal == 0
        then fmap (fmap (const 0)) keyWeights
        else fmap (fmap ((% weightTotal) . fromIntegral)) keyWeights

      -- Multiply each percentage by the total vote power.
      portion :: NonEmpty (key, Ratio Integer)
      portion = fmap (fmap (* (powerTotal % 1))) percentage

      -- Round the voting power down.
      floored :: NonEmpty (key, Integer)
      floored = fmap (fmap floor) portion

      -- Assign remaining vote power to final key.
      flooredVotePower :: Integer
      flooredVotePower = sum $ fmap snd floored

      remainingVotePower :: Integer
      remainingVotePower = powerTotal - flooredVotePower
    in
      case (NE.init floored, NE.last floored) of
        (initial, (lastVotePub, lastPower)) ->
          NE.fromList $
            initial <> [(lastVotePub, lastPower + remainingVotePower)]
