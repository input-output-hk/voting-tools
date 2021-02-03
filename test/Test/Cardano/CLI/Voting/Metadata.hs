{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.CLI.Voting.Metadata where

import           Data.Either
import           Data.List (delete, find, sort)
import           Data.Monoid (Sum (Sum), getSum)
import           Data.Maybe
import qualified Data.ByteString.Base16 as Base16
import           Data.Word (Word8)
import           Hedgehog (Gen, MonadTest, Property, annotate, forAll, property, tripping, (===))
import           Hedgehog.Internal.Property (forAllT)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit (Assertion, assertEqual, testCase, (@?=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.API as Api

import           Cardano.CLI.Voting.Signing (AsType (AsVoteVerificationKey))
import qualified Cardano.API.Extended as Api
import qualified Test.Generators as Gen
import Cardano.CLI.Voting.Metadata

tests :: TestTree
tests = testGroup "Vote Metadata type tests"
  [ testGroup "Parsers and printers"
      [ testCase "TxMetadata/decode-eg" unit_txMetadata_can_decode_example
      -- , testProperty "TxMetadata/json/roundtrips" prop_txMetadata_json_roundtrips 
      , testCase "Vote/decode-eg" unit_vote_can_decode_example
      , testProperty "Vote/toTxMetadata/fromTxMetadata/roundtrips" prop_vote_json_roundtrips 
      ]
  ]

-- prop_txMetadata_json_roundtrips = property $ do
--   a <- forAll Gen.txMetadata

--   tripping (sortMetaMaps a) metadataToJson (fmap sortMetaMaps . parseMetadataFromJson)

prop_vote_json_roundtrips = property $ do
  a <- forAllT Gen.vote

  tripping a voteToTxMetadata voteFromTxMetadata

unit_txMetadata_can_decode_example = do
  let
    jsonMetadata = Aeson.Object $ HM.fromList
      [ ("61284", Aeson.Object $ HM.fromList
          [ ("1", Aeson.String "0x49b8a147e4ffb1119d460feef2d13a9e882684f30f8cf74e6956246670b2652e")
          , ("2", Aeson.String "0xc14fad1da753e2701b9d3546ace0bffe97670598b0ed53f63484c7ded732a0a9")
          , ("3", Aeson.String "0x009d78cbfe0ec5b263d96847fad6b988c5edddc013aa3af83148e2f9af67cdce358308a9a132b87fc6ed005b36261b081e65f3213eead7eb07")
          ])
      , ("61285", Aeson.Object $ HM.fromList
          [ ("1", Aeson.String "0xfb01d767515ad75a959ef1b154bfb704c1a2a1af9e8c36ea38caade4931f9967780f08cfb2dfdac92e0b1efcca0cb148587b656007e87f1af0be3d4a93826706") ])
      ]
     
  parseMetadataFromJson jsonMetadata
    @?= (Right $ Api.makeTransactionMetadata $ M.fromList
          [ (61284, Api.TxMetaMap [ (Api.TxMetaNumber 1, Api.TxMetaBytes $ fromRight "" $ Base16.decode "49b8a147e4ffb1119d460feef2d13a9e882684f30f8cf74e6956246670b2652e")
                                  , (Api.TxMetaNumber 2, Api.TxMetaBytes $ fromRight "" $ Base16.decode "c14fad1da753e2701b9d3546ace0bffe97670598b0ed53f63484c7ded732a0a9")
                                  , (Api.TxMetaNumber 3, Api.TxMetaBytes $ fromRight "" $ Base16.decode "009d78cbfe0ec5b263d96847fad6b988c5edddc013aa3af83148e2f9af67cdce358308a9a132b87fc6ed005b36261b081e65f3213eead7eb07")
                                  ]
            )
          , (61285, Api.TxMetaMap [ (Api.TxMetaNumber 1, Api.TxMetaBytes $ fromRight "" $ Base16.decode "fb01d767515ad75a959ef1b154bfb704c1a2a1af9e8c36ea38caade4931f9967780f08cfb2dfdac92e0b1efcca0cb148587b656007e87f1af0be3d4a93826706")])
          ]
        )

unit_vote_can_decode_example = do
  let
    b1 = fromRight (error "b1") $ Base16.decode "49b8a147e4ffb1119d460feef2d13a9e882684f30f8cf74e6956246670b2652e"
    b2 = fromRight (error "b2") $ Base16.decode "c14fad1da753e2701b9d3546ace0bffe97670598b0ed53f63484c7ded732a0a9"
    b3 = fromRight (error "b3") $ Base16.decode "009d78cbfe0ec5b263d96847fad6b988c5edddc013aa3af83148e2f9af67cdce358308a9a132b87fc6ed005b36261b081e65f3213eead7eb07"
    b4 = fromRight (error "b4") $ Base16.decode "fb01d767515ad75a959ef1b154bfb704c1a2a1af9e8c36ea38caade4931f9967780f08cfb2dfdac92e0b1efcca0cb148587b656007e87f1af0be3d4a93826706"
    txMetadata = Api.makeTransactionMetadata $ M.fromList
      [ (61284, Api.TxMetaMap [ (Api.TxMetaNumber 1, Api.TxMetaBytes b1 )
                              , (Api.TxMetaNumber 2, Api.TxMetaBytes b2 )
                              , (Api.TxMetaNumber 3, Api.TxMetaBytes b3 )
                              ]
        )
      , (61285, Api.TxMetaMap [ (Api.TxMetaNumber 1, Api.TxMetaBytes b4 )])
      ]

  let
    expectedSig         = fromMaybe (error "sig") $ Crypto.rawDeserialiseSigDSIGN b4
    expectedVotePub     = fromMaybe (error "votepub") $ Api.deserialiseFromRawBytes Api.AsVotingKeyPublic b1
    expectedStkVerify   = fromMaybe (error "stkVerify") $ Api.deserialiseFromRawBytes AsVoteVerificationKey b2
    expectedPaymentAddr = fromMaybe (error "paymentAddr") $ Api.deserialiseFromRawBytes Api.AsAddressAny b3
    expected            = fromMaybe (error "expected") $ (mkVotePayload expectedVotePub expectedStkVerify expectedPaymentAddr) `signVotePayload` expectedSig

  voteFromTxMetadata txMetadata @?= (Right expected)

sortMetaMap :: Api.TxMetadataValue -> Api.TxMetadataValue
sortMetaMap (Api.TxMetaMap xs) = (Api.TxMetaMap $ sort xs)
sortMetaMap x              = x

sortMetaMaps :: Api.TxMetadata -> Api.TxMetadata
sortMetaMaps (Api.TxMetadata m) = (Api.TxMetadata $ fmap sortMetaMap m)
