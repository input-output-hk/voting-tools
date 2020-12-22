
module Test.Generators ( lovelace
                       ) where


import           Control.Monad.Except
import           Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import           Hedgehog (Gen, Property, forAll, property, tripping, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog

import           Cardano.API (Lovelace (Lovelace), deserialiseFromRawBytes)
import qualified Data.Aeson as Aeson

import           Cardano.API.Extended (AsType (AsVotingKeyPublic), VotingKeyPublic)
import           Cardano.CLI.Fetching

-- votingFunds :: Gen VotingFunds
-- votingFunds = VotingFunds <$> Gen.map (Range.linear 0 16) ((,) <$> jaddr <*> lovelace)

-- Gen valid Bech32
-- https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#bech32

-- votingKeyPublic :: Gen VotingKeyPublic
-- votingKeyPublic = do
--   bs <- Gen.bytes (Range.linear 0 256)
--   case deserialiseFromRawBytes AsVotingKeyPublic bs of
--     Nothing  -> fail $ "Failed to create VotingKeyPublic from raw bytes: " <> show bs
--     Just key -> pure key

-- jaddr :: Gen JormungandrAddress

lovelace :: Gen Lovelace
lovelace = (Lovelace . toInteger) <$> Gen.int64 (Range.linear 0 maxBound)
