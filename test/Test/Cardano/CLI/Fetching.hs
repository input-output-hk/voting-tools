module Test.Cardano.CLI.Fetching
  ( tests
  )
where

import           Cardano.CLI.Fetching ()
import qualified Data.Aeson as Aeson
import           Hedgehog (Property, property, tripping)
import           Hedgehog.Internal.Property (forAllT)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import qualified Test.Generators as Gen

tests :: TestTree
tests = testGroup "Test.Cardano.CLI.Fetching"
  [ testProperty "JSON roundtrip VotingPower" prop_votingPower_json_roundtrip
  ]

prop_votingPower_json_roundtrip :: Property
prop_votingPower_json_roundtrip = property $ do
  votePower <- forAllT Gen.votingPower
  tripping votePower Aeson.encode Aeson.eitherDecode'
