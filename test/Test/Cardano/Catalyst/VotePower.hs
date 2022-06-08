{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Catalyst.VotePower
  ( tests
  )
where

import           Cardano.Catalyst.Registration (Delegations (..), Vote, delegations)
import           Cardano.Catalyst.VotePower
import qualified Data.Aeson as Aeson
import           Hedgehog (Property, annotate, cover, property, tripping, (===))
import           Hedgehog.Internal.Property (forAllT)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, testCase, (@?=))
import           Test.Tasty.Hedgehog (testProperty)

import qualified Cardano.Catalyst.Test.DSL.Gen as Gen
import qualified Cardano.Catalyst.Test.VotePower.Gen as Gen
import qualified Data.List.NonEmpty as NE
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: TestTree
tests = testGroup "Test.Cardano.Catalyst.VotePower"
  [ testProperty "JSON roundtrip VotingPower" prop_votingPower_json_roundtrip
  , testProperty "delegation value conserved" prop_delegateVotingPower_value_conserved
  , testProperty "value conserved" prop_registration_value_conserved
  , testProperty "values conserved" prop_registration_values_conserved
  , testCase "delegation assigns remaining voting power to final vote" test_remainder_votingPower
  , testCase "delegation assigns remaining voting power to final vote when weights are zero" test_remainder_votingPower_zero
  , testCase "delegation assigns voting power correctly" test_votingPower
  ]

prop_votingPower_json_roundtrip :: Property
prop_votingPower_json_roundtrip = property $ do
  votePower <- forAllT Gen.votingPower
  tripping votePower Aeson.encode Aeson.eitherDecode'

-- | Voting power is assigned correctly.
test_votingPower :: Assertion
test_votingPower =
  let
    -- Given a voting power of 12,
    power = 12

    -- and a total voting weight of 6, we expect the voting power to be assigned
    -- in the following manner:
    --   [(a, 6), (b, 2), (c, 4)]
    delegate = Delegations . NE.fromList $
      [ ( 'a', 3 )
      , ( 'b', 1 )
      , ( 'c', 2 )
      ]
  in
    -- Hence our expectation:
    delegateVotingPower delegate power
      @?= NE.fromList [ ('a', 6)
                      , ('b', 2)
                      , ('c', 4)
                      ]

-- | When voting power is rounded down, remainder is assigned to last
-- delegation.
test_remainder_votingPower :: Assertion
test_remainder_votingPower =
  let
    -- Given a voting power of 3,
    power = 3

    -- and a total voting weight of 4, we expect the voting power to be assigned
    -- in the following manner:
    --   [(a, 0.75), (b, 0.75), (c, 1.5)]
    -- However, we know from the spec
    -- (https://github.com/cardano-foundation/CIPs/blob/1cc035d873082c39c7e3b1faf3204c552e34d5a1/CIP-0036/README.md#associating-voting-power-with-a-voting-key)
    -- that fractional ADA amounts are rounded down, giving us:
    --   [(a, 0), (b, 0), (c, 1)]
    -- But we also know that the remaining vote power must be associated with
    -- the last delegation:
    --   [(a, 0), (b, 0), (c, 3)]
    delegate = Delegations . NE.fromList $
      [ ( 'a', 1 )
      , ( 'b', 1 )
      , ( 'c', 2 )
      ]
  in
    -- Hence our expectation:
    delegateVotingPower delegate power
      @?= NE.fromList [ ('a', 0)
                      , ('b', 0)
                      , ('c', 3)
                      ]

test_remainder_votingPower_zero :: Assertion
test_remainder_votingPower_zero =
  let
    -- Given a voting power of 10,
    power = 10

    -- when the sum of weights is zero, we expect the voting power to be
    -- assigned to the last key.
    -- This ensures that total voting power is maintained despite a poor choice
    -- in weights.
    delegate = Delegations . NE.fromList $
      [ ( 'a', 0 )
      , ( 'b', 0 )
      ]
  in
    delegateVotingPower delegate power
      @?= NE.fromList [ ( 'a', 0)
                      , ( 'b', 10)
                      ]

prop_delegateVotingPower_value_conserved :: Property
prop_delegateVotingPower_value_conserved = property $ do
  delegs <- forAllT Gen.genDelegations
  cover 10 "zero weights" $
    and $ (\(_key, weight) -> weight == 0) <$> delegations delegs
  cover 30 "non-zero weights" $
    and $ (\(_key, weight) -> weight /= 0) <$> delegations delegs
  cover 10 "legacy delegations" $
    case delegs of
      (LegacyDelegation _) -> True
      _ -> False
  cover 10 "new delegations" $
    case delegs of
      (Delegations _) -> True
      _ -> False
  cover 10 "one delegation" $ length (delegations delegs) == 1
  cover 10 "many delegations" $ length (delegations delegs) > 3
  power <- forAllT $ fromIntegral <$> Gen.int64 (Range.linearBounded)

  let
    delegated = delegateVotingPower delegs power
    delegatedSum = sum $ fmap snd delegated

  -- Negative power should be clamped to zero.
  if power >= 0
  then delegatedSum === power
  else delegatedSum === 0

prop_registration_value_conserved :: Property
prop_registration_value_conserved = property $ do
  rego <- forAllT $ Gen.genVote
  power <- forAllT $ fromIntegral <$> Gen.int64 (Range.linearBounded)

  let
    votingPower :: VotingPower
    votingPower = votingPowerFromRegoValue rego power

    votingPowerSum :: Integer
    votingPowerSum = _powerVotingPower votingPower

  annotate $ "votingPower: " <> show votingPower

  if power >= 0
  then votingPowerSum === power
  else votingPowerSum === 0

prop_registration_values_conserved :: Property
prop_registration_values_conserved = property $ do
  (regoValues :: [(Vote, Integer)]) <-
    forAllT $ Gen.list (Range.linear 0 15) $ do
      rego <- Gen.genVote
      power <- fromIntegral <$> Gen.int64 (Range.linearBounded)
      pure (rego, power)

  let
    -- Negative values should not be considered when summing.
    clampNegative :: Integer -> Integer -> Integer
    clampNegative clamp v = if v < 0 then clamp else v

    -- Sum total power of registrations (ignoring negative power).
    regoValuesSum :: Integer
    regoValuesSum = sum $ fmap (clampNegative 0 . snd) regoValues

    -- Run function under test.
    votingPower :: [VotingPower]
    votingPower = votingPowerFromRegoValues regoValues

    -- Get the sum of the voting power.
    votingPowerSum :: Integer
    votingPowerSum = sum $ fmap (_powerVotingPower) votingPower

  annotate $ "votingPower: " <> show votingPower

  -- Ensure total power of registrations matches voting power sum.
  votingPowerSum === regoValuesSum
