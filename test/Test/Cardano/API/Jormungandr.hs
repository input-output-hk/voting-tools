module Test.Cardano.API.Jormungandr
  ( tests
  )
where

import           Cardano.API.Jormungandr (addressFromVotingKeyPublic, votingKeyPublicFromAddress)
import           Cardano.Api (NetworkId (..))
import           Hedgehog (Property, forAll, property, tripping)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import qualified Test.Generators as Gen

tests :: TestTree
tests = testGroup "Test.Cardano.API.Jormungandr"
  [ testProperty "VotingKeyPublic/Jormungandr.Address roundtrip" prop_votingKeyPublic_jormungandrAddress_roundtrip
  ]

prop_votingKeyPublic_jormungandrAddress_roundtrip :: Property
prop_votingKeyPublic_jormungandrAddress_roundtrip = property $ do
  votepub <- forAll Gen.votingKeyPublic
  tripping votepub (addressFromVotingKeyPublic Mainnet) (votingKeyPublicFromAddress)
