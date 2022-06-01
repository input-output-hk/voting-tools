{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Catalyst.Registration where

import qualified Cardano.Api as Api
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import           Data.Either
import qualified Data.HashMap.Strict as HM
import           Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Text.Encoding as T
import           Hedgehog (property, tripping, (===))
import qualified Hedgehog as H
import           Hedgehog.Internal.Property (forAllT)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, testCase, (@?=))
import           Test.Tasty.Hedgehog

import           Cardano.Catalyst.Registration
import qualified Cardano.Catalyst.Registration.Types.Purpose as Purpose
import qualified Cardano.Catalyst.Test.DSL.Gen as Gen

tests :: TestTree
tests = testGroup "Vote Metadata type tests"
  [ testGroup "Parsers and printers"
      [ testCase "TxMetadata/decode-eg" unit_txMetadata_can_decode_example
      , testProperty "Vote/txMetadata/format" prop_vote_serialized_format
      , testProperty "Vote/toTxMetadata/fromTxMetadata/roundtrips" prop_vote_txMetadata_roundtrips
      , testProperty "JSON roundrip RewardsAddress" prop_rewardsAddress_json_roundtrips
      ]
  , testGroup "Purpose"
    [ testProperty "Purpose/txMetadata/roundtrip" prop_purpose_txMetadata_roundtrip
    , testProperty "Purpose/json/roundtrip" prop_purpose_json_roundtrip
    , testProperty "Purpose/purposeNumber/roundtrip" prop_purpose_number_roundtrip
    , testCase "Purpose/purposeNumber/expected" test_purpose_number_format
    , testCase "Purpose/metadataFormat/expected" test_purpose_metadata_format
    , testCase "Purpose/jsonFormat/expected" test_purpose_json_format
    ]
  ]

prop_rewardsAddress_json_roundtrips :: H.Property
prop_rewardsAddress_json_roundtrips = property $ do
  rewardsAddr <- forAllT Gen.genRewardsAddress
  tripping rewardsAddr Aeson.encode Aeson.eitherDecode'

prop_vote_txMetadata_roundtrips :: H.Property
prop_vote_txMetadata_roundtrips = property $ do
  a <- forAllT Gen.genVote

  tripping a voteToTxMetadata voteFromTxMetadata

unit_txMetadata_can_decode_example :: Assertion
unit_txMetadata_can_decode_example = do
  let
    jsonMetadata = Aeson.Object $ HM.fromList
      [ ("61284", Aeson.Object $ HM.fromList
          [ ("1", Aeson.String "0x49b8a147e4ffb1119d460feef2d13a9e882684f30f8cf74e6956246670b2652e")
          , ("2", Aeson.String "0xc14fad1da753e2701b9d3546ace0bffe97670598b0ed53f63484c7ded732a0a9")
          , ("3", Aeson.String "0x009d78cbfe0ec5b263d96847fad6b988c5edddc013aa3af83148e2f9af67cdce358308a9a132b87fc6ed005b36261b081e65f3213eead7eb07")
          , ("4", Aeson.Number $ fromIntegral (8 :: Int))
          ])
      , ("61285", Aeson.Object $ HM.fromList
          [ ("1", Aeson.String "0xfb01d767515ad75a959ef1b154bfb704c1a2a1af9e8c36ea38caade4931f9967780f08cfb2dfdac92e0b1efcca0cb148587b656007e87f1af0be3d4a93826706") ])
      ]

  Api.metadataFromJson Api.TxMetadataJsonNoSchema jsonMetadata
    @?= (Right $ Api.makeTransactionMetadata $ M.fromList
          [ (61284, Api.TxMetaMap [ (Api.TxMetaNumber 1, Api.TxMetaBytes $ fromRight "" $ Base16.decode "49b8a147e4ffb1119d460feef2d13a9e882684f30f8cf74e6956246670b2652e")
                                  , (Api.TxMetaNumber 2, Api.TxMetaBytes $ fromRight "" $ Base16.decode "c14fad1da753e2701b9d3546ace0bffe97670598b0ed53f63484c7ded732a0a9")
                                  , (Api.TxMetaNumber 3, Api.TxMetaBytes $ fromRight "" $ Base16.decode "009d78cbfe0ec5b263d96847fad6b988c5edddc013aa3af83148e2f9af67cdce358308a9a132b87fc6ed005b36261b081e65f3213eead7eb07")
                                  , (Api.TxMetaNumber 4, Api.TxMetaNumber 8)
                                  ]
            )
          , (61285, Api.TxMetaMap [ (Api.TxMetaNumber 1, Api.TxMetaBytes $ fromRight "" $ Base16.decode "fb01d767515ad75a959ef1b154bfb704c1a2a1af9e8c36ea38caade4931f9967780f08cfb2dfdac92e0b1efcca0cb148587b656007e87f1af0be3d4a93826706")])
          ]
        )

-- | This test simply checks that the vote -> txMetadata -> json
-- function results in a JSON format we expect. Via
-- 'prop_vote_txMetadata_roundtrips' we have already proven that the
-- format parses.
prop_vote_serialized_format :: H.Property
prop_vote_serialized_format = H.property $ do
  vote <- forAllT $ Gen.genVote

  let
    sig         = ("0x" <>) . T.decodeUtf8 . Base16.encode . Crypto.rawSerialiseSigDSIGN . voteSignature $ vote
    votePub     = ("0x" <>) . T.decodeUtf8 . Api.serialiseToRawBytesHex . voteRegistrationPublicKey $ vote
    verKey      = ("0x" <>) . T.decodeUtf8 . Api.serialiseToRawBytesHex . voteRegistrationVerificationKey $ vote
    rewardsAddr = ("0x" <>) . T.decodeUtf8 . Api.serialiseToRawBytesHex . voteRegistrationRewardsAddress $ vote
    slotNo      = fromIntegral . voteRegistrationSlot $ vote

    expectedJSON = Aeson.Object $ HM.fromList
      [ ( "61285", Aeson.Object $ HM.fromList [ ("1", Aeson.String sig) ] )
      , ( "61284", Aeson.Object $ HM.fromList $
          [ ("1", Aeson.String votePub)
          , ("2", Aeson.String verKey)
          , ("3", Aeson.String rewardsAddr)
          , ("4", Aeson.Number slotNo)
          ] ++ case voteRegistrationPurpose vote of
            Nothing -> []
            Just p  -> [("5", Aeson.toJSON p)]
        )
      ]

  Api.metadataToJson Api.TxMetadataJsonNoSchema (voteToTxMetadata vote)
    === expectedJSON

sortMetaMap :: Api.TxMetadataValue -> Api.TxMetadataValue
sortMetaMap (Api.TxMetaMap xs) = (Api.TxMetaMap $ sort xs)
sortMetaMap x              = x

sortMetaMaps :: Api.TxMetadata -> Api.TxMetadata
sortMetaMaps (Api.TxMetadata m) = (Api.TxMetadata $ fmap sortMetaMap m)

prop_purpose_txMetadata_roundtrip :: H.Property
prop_purpose_txMetadata_roundtrip = property $ do
  purpose <- forAllT Gen.genPurpose
  tripping purpose Purpose.toTxMetadataValue Purpose.fromTxMetadataValue

prop_purpose_json_roundtrip :: H.Property
prop_purpose_json_roundtrip = property $ do
  purpose <- forAllT Gen.genPurpose
  tripping purpose Aeson.encode Aeson.eitherDecode'

prop_purpose_number_roundtrip :: H.Property
prop_purpose_number_roundtrip = property $ do
  purpose <- forAllT Gen.genPurpose
  tripping purpose purposeNumber mkPurpose

test_purpose_number_format :: Assertion
test_purpose_number_format = do
  purposeNumber catalystPurpose @?= 0
  purposeNumber <$> (mkPurpose 3) @?= Right 3
  purposeNumber <$> (mkPurpose (-1))
    @?= Left "expected a positive integer, got a negative integer"

test_purpose_metadata_format :: Assertion
test_purpose_metadata_format = do
  Purpose.toTxMetadataValue catalystPurpose @?= Api.TxMetaNumber 0
  Purpose.toTxMetadataValue <$> (mkPurpose 10) @?= Right (Api.TxMetaNumber 10)

test_purpose_json_format :: Assertion
test_purpose_json_format = do
  Aeson.encode catalystPurpose @?= "0"
  Aeson.encode <$> (mkPurpose 10) @?= Right "10"
