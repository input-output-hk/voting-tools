
module Cardano.Catalyst.Test.VotePower.Gen where

import           Cardano.Catalyst.Test.DSL.Gen (genRewardsAddress, genStakeVerificationKey,
                   genVotingKeyPublic)
import           Cardano.Catalyst.VotePower (VotingPower (..))
import           Control.Monad.IO.Class (MonadIO)
import           Hedgehog (MonadGen)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

votingPower :: (MonadGen m, MonadIO m) => m VotingPower
votingPower =
  VotingPower
  <$> genVotingKeyPublic
  <*> genStakeVerificationKey
  <*> genRewardsAddress
  <*> (fromIntegral <$> Gen.word64 Range.constantBounded)
