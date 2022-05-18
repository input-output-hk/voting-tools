{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Catalyst.Presentation where

import           Cardano.CLI.Voting.Metadata
import           Cardano.CLI.Voting.Signing
import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import           Data.Char (toLower)
import           GHC.Generics

import           Cardano.API.Extended (VotingKeyPublic)

scaleRegistrationAmt :: Int -> [(a, Integer)] -> [(a, Integer)]
scaleRegistrationAmt scale = fmap (fmap (`div` fromIntegral scale))

data RegistrationInfo
  = RegistrationInfo { _regoInfoRego :: Vote
                     , _regoInfoAmount :: Integer
                     }
  deriving (Eq, Ord, Show)

votingPowerFromRegistrationInfo :: Int -> RegistrationInfo -> VotingPower
votingPowerFromRegistrationInfo scale (RegistrationInfo rego amt) =
  let
    power = (amt `div` fromIntegral scale)

    votePub = voteRegistrationPublicKey rego
    stakePub = voteRegistrationVerificationKey rego
    rewardAddr = voteRegistrationRewardsAddress rego
  in
    VotingPower votePub stakePub rewardAddr power

data VotingPower
  = VotingPower { _powerVotingPublicKey :: VotingKeyPublic
                , _powerStakePublicKey :: StakeVerificationKey
                , _powerRewardAddress :: RewardsAddress
                , _powerVotingPower:: Integer
                }
  deriving (Eq, Show, Generic)

votingPowerJsonParserOptions :: Aeson.Options
votingPowerJsonParserOptions = Aeson.defaultOptions
    { Aeson.fieldLabelModifier = fmap toLower . Aeson.camelTo2 '_' . (drop 6) }

instance ToJSON VotingPower where
  toJSON = Aeson.genericToJSON votingPowerJsonParserOptions

instance FromJSON VotingPower where
  parseJSON = Aeson.genericParseJSON votingPowerJsonParserOptions

jsonParserOptions :: Aeson.Options
jsonParserOptions = Aeson.defaultOptions { Aeson.fieldLabelModifier = (fmap toLower) . (drop 2)
                                         }
