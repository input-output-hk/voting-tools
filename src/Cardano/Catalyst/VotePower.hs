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
import           Cardano.Catalyst.Registration
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Char (toLower)
import           Data.List (nub)
import           GHC.Generics (Generic)

import qualified Cardano.Api as Api
import qualified Data.Aeson as Aeson

data VotingPower
  = VotingPower { _powerVotingKey      :: VotingKeyPublic
                , _powerStakePublicKey :: StakeVerificationKey
                , _powerRewardsAddress :: RewardsAddress
                , _powerVotingPower    :: Integer
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
  nub $ foldMap ((:[]) . uncurry votingPowerFromRegoValue) regoValues

votingPowerFromRegoValue :: Vote -> Integer -> VotingPower
votingPowerFromRegoValue rego power =
  let
    votePub = voteRegistrationPublicKey rego
    stakeKey    = voteRegistrationVerificationKey rego
    rewardsAddr = voteRegistrationRewardsAddress rego
  in
    VotingPower votePub stakeKey rewardsAddr power
