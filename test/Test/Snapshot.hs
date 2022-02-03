{-# LANGUAGE QuasiQuotes #-}

module Test.Snapshot
  ( tests
  )
where

import           Cardano.API.Extended (VotingKeyPublic)
import           Cardano.API.Jormungandr (addressFromVotingKeyPublic)
import           Cardano.Api (NetworkId (..))
import           Control.Lens (at, (&), (^..), (^?!))
import           Data.Aeson.Lens (_Object, values)
import           Data.Aeson.QQ (aesonQQ)
import           Data.List (sortOn)
import           Data.Maybe (fromJust)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, testCase, (@?=))

import           Snapshot

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM

tests :: TestTree
tests = testGroup "Test.Snapshot"
  [ testCase "Funds from snapshot template" unit_funds_from_snapshot_template
  , testCase "Funds from new output" unit_funds_from_new_output
  ]

unit_funds_from_snapshot_template :: Assertion
unit_funds_from_snapshot_template = do
  getAllFundContents exampleSnapshot @?= Aeson.toJSON (sorted $ fund1 <> fund2)

  where
    sorted = sortOn (^?! at "address")

unit_funds_from_new_output :: Assertion
unit_funds_from_new_output = do
  getSnapshotContents Mainnet exampleNewOutput @?= Aeson.toJSON (sorted $ expectedOutput)

  where
    sorted x =
      x & (^.. values)
        & sortOn (^?! _Object . at "address")

fund1 :: [ Aeson.Object ]
fund1 = [ HM.fromList [ ("address", Aeson.String "ca1qwhhj0gdtrmeuacud5ghftg7krk0yx28rpf5s3j22cehp0kypk0h2yduhqs")
                      , ("value", Aeson.Number 47979864603)
                      ]
        , HM.fromList [ ("address", Aeson.String "ca1q0z9mdq95r56f9g587ehvfhqal02vqpwa72z0rw45z28mxhqg2wtwdut504")
                      , ("value", Aeson.Number 26927101029)
                      ]
        ]

fund2 :: [ Aeson.Object ]
fund2 = [ HM.fromList [ ("address", Aeson.String "ca1q5mvekdfq3mgeszpyv5hyefh4g2leskgrdrpgf7drqmdsuav73c4stsee6c")
                      , ("value", Aeson.Number 30000000003)
                      ]
        , HM.fromList [ ("address", Aeson.String "ca1q4s3f5n9975egq55j5sldvqglxg27emua3f47g8jlt0s2qt8crvrsqsmxtz")
                      , ("value", Aeson.Number 30000000006)
                      ]
        , HM.fromList [ ("address", Aeson.String "ca1qkzxxn6wk8rf46quhe4hx64a24dztr9dh0dj4mc26c2nlctr692kztqnadr")
                      , ("value", Aeson.Number 30000000006)
                      ]
        ]

exampleSnapshot :: Aeson.Value
exampleSnapshot = [aesonQQ|
    {
      "blockchain_configuration": {
        "block0_date": 1605546000,
        "discrimination": "production",
        "block0_consensus": "bft",
        "consensus_leader_ids": [
          "ed25519_pk169pp82fcgn9mzaxumempmjnqpyqzhgr4xfzs4t85xkjm4z4hrjqsnpq4ke",
          "ed25519_pk1m7xre5p7txtuecc94nnvgcl950kjahu4mzv9h0nn0g5d434t503s3a3ggn",
          "ed25519_pk1xmxdn2gyw6xvqsfr99ex2da2zh7v9jqmgc2z0ngcxmv88t85w9vqppjqd4"
        ],
        "linear_fees": {
          "constant": 0,
          "coefficient": 0,
          "certificate": 0
        },
        "slots_per_epoch": 4320,
        "slot_duration": 20,
        "block_content_max_size": 102400,
        "epoch_stability_depth": 102400,
        "committees": [
          "84634f4eb1c69ae81cbe6b736abd555a258cadbbdb2aef0ad6153fe163d15561",
          "6114d2652fa99402949521f6b008f990af677cec535f20f2fadf050167c0d838",
          "b83a1a1a8c69f913ae04dca5eb25b723e81d7016cd57a438637361d11c47e104"
        ]
      },
      "initial": [
          {
              "fund": #{fund1}
          },
          {
              "fund": #{fund2}
          }
      ]
    }
  |]

votingKey1 :: VotingKeyPublic
votingKey1 = fromJust $ Aeson.decode' "\"0xc21ddb4abb04bd5ce21091eef1676e44889d806e6e1a6a9a7dc25c0eba54cc33\""

votingKey2 :: VotingKeyPublic
votingKey2 = fromJust $ Aeson.decode' "\"0x3f656a1ba4ea8b33c81961fee6f15f09600f024435b1a7ada1e5b77b03a41a6d\""

votingKey3 :: VotingKeyPublic
votingKey3 = fromJust $ Aeson.decode' "\"0x125860fc4870bb480d1d2a97f101e1c5c845c0222400fdaba7bcca93e79bd66e\""

exampleNewOutput :: Aeson.Value
exampleNewOutput = [aesonQQ|
    [
        {
            "reward_address": "0xe1ffff2912572257b59dca84c965e4638a09f1524af7a15787eb0d8a46",
            "stake_public_key": "e7d6616840734686855ec80ee9658f5ead9e29e494ec6889a5d1988b50eb8d0f",
            "voting_power": 177689370111,
            "voting_public_key": #{votingKey1}
        },
        {
            "reward_address": "0xe1ffff2912572257b59dca84c965e4638a09f1524af7a15787eb0d8a46",
            "stake_public_key": "e7d6616840734686855ec80ee9658f5ead9e29e494ec6889a5d1988b50eb8d0f",
            "voting_power": 300,
            "voting_public_key": #{votingKey1}
        },
        {
            "reward_address": "0xe1fffc8bcb1578a15413bf11413639fa270a9ffa36d9a0c4d2c93536fe",
            "stake_public_key": "2f9a90d87321a255efd038fea5df2a2349ea2c32fa584b73f2a46f655f235919",
            "voting_power": 9420156337,
            "voting_public_key": #{votingKey2}
        },
        {
            "reward_address": "0xe1fff825e1bf009d35d9160f6340250b581f5d37c17538e960c0410b20",
            "stake_public_key": "66ae1553036548b99b93c783811bb281be5a196a12d950bda4ac9b83630afbd1",
            "voting_power": 82259292916,
            "voting_public_key": #{votingKey3}
        }
    ]
|]

expectedOutput :: Aeson.Value
expectedOutput = [aesonQQ|
    [
        {
            "address": #{addressFromVotingKeyPublic Mainnet votingKey1},
            "value": 177689370411
        },
        {
            "address": #{addressFromVotingKeyPublic Mainnet votingKey2},
            "value": 9420156337
        },
        {
            "address": #{addressFromVotingKeyPublic Mainnet votingKey3},
            "value": 82259292916
        }
    ]
  |]
