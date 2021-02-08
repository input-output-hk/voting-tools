{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.CLI.Fetching
  ( tests
  )
where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as M
import           Hedgehog (Gen, Property, forAll, property, tripping, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit (Assertion, assertEqual, testCase)

import           Cardano.Api (Lovelace, deserialiseFromRawBytes)

import           Cardano.API.Extended (AsType (AsVotingKeyPublic), VotingKeyPublic,
                     deserialiseFromBech32')
import           Cardano.CLI.Fetching
import qualified Test.Generators as Gen

tests :: TestTree
tests = testGroup "Fetching Registrations"
  -- [ testGroup "Property tests"
  --     [ testProperty "VotingFunds JSON parser roundtrips"  prop_votingfunds_json_roundtrip
  --     , testProperty "VotingFunds semigroup/associativity" prop_votingfunds_associativity
  --     , testProperty "VotingFunds monoid/identity"         prop_votingfunds_identity
  --     , testProperty "VotingFunds monoid/concatenation"    prop_votingfunds_concat
  --     ]
  [ testGroup "Unit tests"
      []
      -- [ testCase "Can read JormungandrAddress from Bech32" unit_jaddr_bech32_read
      -- , testCase "Can read JormungandrAddress from JSON" unit_jaddr_json_read
      -- , testCase "Can read VotingFunds from JSON" unit_votingfunds_json_read
      -- ]
  ]

-- unit_jaddr_bech32_read :: Assertion
-- unit_jaddr_bech32_read =
--   let
--     bech32 = "ca1qhg5yyaf8pzvhvt5mn08v8w2vqysq2aqw5ey2z4v7s66tw52kuwgzx7z7v5"
--   in
--     assertEqual
--       "Failed to deserialise JormungandrAddress from Bech32"
--       (Right "\ENQ\209B\DC3\169\&8D\203\177t\220\222v\GS\202`\t\NUL+\160u2E\n\172\244\&5\165\186\138\183\FS\129")
--       (jAddrBytes <$> deserialiseFromBech32' AsJormungandrAddress bech32)

-- unit_jaddr_json_read :: Assertion
-- unit_jaddr_json_read =
--   let
--     json = "\"ca1qhg5yyaf8pzvhvt5mn08v8w2vqysq2aqw5ey2z4v7s66tw52kuwgzx7z7v5\""
--   in
--     assertEqual
--       "Failed to deserialise JormungandrAddress from JSON"
--       (Right "\ENQ\209B\DC3\169\&8D\203\177t\220\222v\GS\202`\t\NUL+\160u2E\n\172\244\&5\165\186\138\183\FS\129")
--       (jAddrBytes <$> Aeson.eitherDecode' json)

-- unit_votingfunds_json_read :: Assertion
-- unit_votingfunds_json_read =
--   let
--     json = "{ \"ca1qhg5yyaf8pzvhvt5mn08v8w2vqysq2aqw5ey2z4v7s66tw52kuwgzx7z7v5\": 10000000001,\"ca1qh0cc0xs8eve0n8rqkkwd3rruk376tkljhvfska7wdaz3kkx4w37x2l35z9\": 10000000001,\"ca1q5mvekdfq3mgeszpyv5hyefh4g2leskgrdrpgf7drqmdsuav73c4stsee6c\": 10000000001,\"ca1qkzxxn6wk8rf46quhe4hx64a24dztr9dh0dj4mc26c2nlctr692kztqnadr\": 10000000002,\"ca1q4s3f5n9975egq55j5sldvqglxg27emua3f47g8jlt0s2qt8crvrsqsmxtz\": 10000000002,\"ca1qkur5xs6335ljyawqnw2t6e9ku37s8tszmx40fpcvdekr5guglssg3gf87s\": 10000000002 }"
--     votingFundsToList (VotingFunds m) =
--       (\(k,v) -> (jAddrBytes k, v)) <$> M.toList m
--   in
--     assertEqual
--       "Failed to deserialise VotingFunds from JSON"
--       (Right [ ( "\ENQ6\204\217\169\EOTv\140\192A#)re7\170\NAK\252\194\200\ESCF\DC4'\205\CAN6\216s\172\244qX"                , Lovelace 10000000001)
--              , ( "\ENQa\DC4\210e/\169\148\STX\148\149!\246\176\b\249\144\175g|\236S_ \242\250\223\ENQ\SOHg\192\216\&8"       , Lovelace 10000000002)
--              , ( "\ENQ\132cON\177\198\154\232\FS\190ksj\189UZ%\140\173\187\219*\239\n\214\NAK?\225c\209Ua"                   , Lovelace 10000000002)
--              , ( "\ENQ\184:\SUB\SUB\140i\249\DC3\174\EOT\220\165\235%\183#\232\GSp\SYN\205W\164\&8csa\209\FSG\225\EOT"       , Lovelace 10000000002)
--              , ( "\ENQ\209B\DC3\169\&8D\203\177t\220\222v\GS\202`\t\NUL+\160u2E\n\172\244\&5\165\186\138\183\FS\129"         , Lovelace 10000000001)
--              , ( "\ENQ\223\140<\208>Y\151\204\227\ENQ\172\230\196c\229\163\237.\223\149\216\152[\190sz(\218\198\171\163\227" , Lovelace 10000000001)
--              ]
--       )
--       (votingFundsToList <$> Aeson.eitherDecode' json)


-- prop_votingfunds_json_roundtrip :: Property
-- prop_votingfunds_json_roundtrip = property $ do
--   votingFunds <- forAll Gen.votingFunds
--   tripping votingFunds Aeson.encode (Aeson.eitherDecode')

-- prop_votingfunds_associativity :: Property
-- prop_votingfunds_associativity = property $ do
--   x <- forAll Gen.votingFunds
--   y <- forAll Gen.votingFunds
--   z <- forAll Gen.votingFunds

--   x <> (y <> z) === (x <> y) <> z

-- prop_votingfunds_identity :: Property
-- prop_votingfunds_identity = property $ do
--   x <- forAll Gen.votingFunds

--   -- Right identity
--   x <> mempty === x
--   -- Left identity
--   mempty <> x === x

-- prop_votingfunds_concat :: Property
-- prop_votingfunds_concat = property $ do
--   xs <- forAll (Gen.list (Range.linear 0 20) Gen.votingFunds)

--   mconcat xs === foldr (<>) mempty xs
