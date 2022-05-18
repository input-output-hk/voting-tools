module Test.Cardano.CLI.Voting.Signing
  ( tests
  )
where

import qualified Data.Aeson as Aeson
import           Hedgehog (Property, property, tripping)
import           Hedgehog.Internal.Property (forAllT)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import qualified Test.Generators as Gen

tests :: TestTree
tests = testGroup "Test.Cardano.CLI.Voting.Signing"
  [ testProperty "JSON roundtrip VoteVerificationKey" prop_voteVerificationKey_json_roundtrip
  ]

prop_voteVerificationKey_json_roundtrip :: Property
prop_voteVerificationKey_json_roundtrip = property $ do
  stakePub <- forAllT Gen.voteVerificationKey
  tripping stakePub Aeson.encode Aeson.eitherDecode'
