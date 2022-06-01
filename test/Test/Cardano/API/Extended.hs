
module Test.Cardano.API.Extended
  ( tests
  )
where

import qualified Data.Aeson as Aeson
import           Hedgehog (Property, forAll, property, tripping)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import qualified Cardano.Catalyst.Test.DSL.Gen as Gen

tests :: TestTree
tests = testGroup "Test.Cardano.API.Extended"
  [ testProperty "JSON roundtrip VotingKeyPublic" prop_votingKeyPublic_json_roundtrip
  ]

prop_votingKeyPublic_json_roundtrip :: Property
prop_votingKeyPublic_json_roundtrip = property $ do
  votepub <- forAll Gen.genVotingKeyPublic
  tripping votepub Aeson.encode Aeson.eitherDecode'
