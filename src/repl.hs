-- This file can be used with ghci src/repl.hs to generate sigs for a given metadata
{-# LANGUAGE OverloadedStrings #-}

module Foo where

import qualified Cardano.Api as Api
import qualified Cardano.API.Extended as Api
import qualified Cardano.Binary as CBOR
import           Cardano.CLI.Voting
import           Cardano.CLI.Voting.Metadata (RewardsAddress, Vote (Vote),
                     VotePayload (VotePayload), mkVotePayload, voteSignature)
import           Cardano.CLI.Voting.Signing (VoteSigningKey, getVoteVerificationKey,
                     hashVotePayload, sign, verificationKeyRawBytes,
                     voteSigningKeyFromStakeSigningKey)
import qualified Cardano.Crypto.DSIGN as Crypto
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import           Data.Either
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)

makeSig :: Integer
makeSig = 1

makeMeta :: Api.TxMetadata
makeMeta = Api.makeTransactionMetadata $ M.fromList
          [ (61284, Api.TxMetaMap [ (Api.TxMetaNumber 1, Api.TxMetaBytes $ fromRight "" $ Base16.decode "49b8a147e4ffb1119d460feef2d13a9e882684f30f8cf74e6956246670b2652e")
                                  , (Api.TxMetaNumber 2, Api.TxMetaBytes $ fromRight "" $ Base16.decode "c14fad1da753e2701b9d3546ace0bffe97670598b0ed53f63484c7ded732a0a9")
                                  , (Api.TxMetaNumber 3, Api.TxMetaBytes $ fromRight "" $ Base16.decode "009d78cbfe0ec5b263d96847fad6b988c5edddc013aa3af83148e2f9af67cdce358308a9a132b87fc6ed005b36261b081e65f3213eead7eb07")
                                  , (Api.TxMetaNumber 4, Api.TxMetaNumber 8)
                                  ]
            )
          ]

makeMetaWithSig :: Api.TxMetadata
makeMetaWithSig = Api.makeTransactionMetadata $ M.fromList
          [ (61284, Api.TxMetaMap [ (Api.TxMetaNumber 1, Api.TxMetaBytes $ fromRight "" $ Base16.decode "49b8a147e4ffb1119d460feef2d13a9e882684f30f8cf74e6956246670b2652e")
                                  , (Api.TxMetaNumber 2, Api.TxMetaBytes $ fromRight "" $ Base16.decode "c14fad1da753e2701b9d3546ace0bffe97670598b0ed53f63484c7ded732a0a9")
                                  , (Api.TxMetaNumber 3, Api.TxMetaBytes $ fromRight "" $ Base16.decode "009d78cbfe0ec5b263d96847fad6b988c5edddc013aa3af83148e2f9af67cdce358308a9a132b87fc6ed005b36261b081e65f3213eead7eb07")
                                  , (Api.TxMetaNumber 4, Api.TxMetaNumber 8)
                                  ]
            )
          , (61285, Api.TxMetaMap [ (Api.TxMetaNumber 1, Api.TxMetaBytes $ fromRight "" $ Base16.decode "fb01d767515ad75a959ef1b154bfb704c1a2a1af9e8c36ea38caade4931f9967780f08cfb2dfdac92e0b1efcca0cb148587b656007e87f1af0be3d4a93826706")])
          ]

makeVoteReg :: Vote
makeVoteReg =
  let stake_skey = fromMaybe (error "BOOM") $ Api.deserialiseFromRawBytesHex (Api.AsSigningKey Api.AsStakeKey) "f5beaeff7932a4164d270afde7716067582412e8977e67986cd9b456fc082e3a" :: Api.SigningKey Api.StakeKey
      vote_stake_skey = voteSigningKeyFromStakeSigningKey stake_skey :: VoteSigningKey
      vote_pubkey = fromMaybe (error "BOOM") $ Api.deserialiseFromRawBytesHex Api.AsVotingKeyPublic "0036ef3e1f0d3f5989e2d155ea54bdb2a72c4c456ccb959af4c94868f473f5a0" :: Api.VotingKeyPublic
      rewards_addr = fromMaybe (error "BOOM") $ Api.deserialiseFromRawBytesHex Api.AsStakeAddress "e0ae3a0a7aeda4aea522e74e4fe36759fca80789a613a58a4364f6ecef" :: RewardsAddress
      slot = 4
  in createVoteRegistration vote_stake_skey vote_pubkey rewards_addr slot

makeVotePayloadCBOR :: ByteString
makeVotePayloadCBOR =
    let
      stake_skey = fromMaybe (error "BOOM") $ Api.deserialiseFromRawBytesHex (Api.AsSigningKey Api.AsStakeKey) "f5beaeff7932a4164d270afde7716067582412e8977e67986cd9b456fc082e3a" :: Api.SigningKey Api.StakeKey
      vote_stake_skey = voteSigningKeyFromStakeSigningKey stake_skey :: VoteSigningKey
      vote_pubkey = fromMaybe (error "BOOM") $ Api.deserialiseFromRawBytesHex Api.AsVotingKeyPublic "0036ef3e1f0d3f5989e2d155ea54bdb2a72c4c456ccb959af4c94868f473f5a0" :: Api.VotingKeyPublic
      rewards_addr = fromMaybe (error "BOOM") $ Api.deserialiseFromRawBytesHex Api.AsStakeAddress "e0ae3a0a7aeda4aea522e74e4fe36759fca80789a613a58a4364f6ecef" :: RewardsAddress
      slot = 4
      payload     = mkVotePayload vote_pubkey (getVoteVerificationKey vote_stake_skey) rewards_addr slot
    in CBOR.serialize' payload

makeVotePayloadHash :: BSC.ByteString
makeVotePayloadHash = hashVotePayload makeVotePayloadCBOR

makeVotePayloadSig =
  let stake_skey = fromMaybe (error "BOOM") $ Api.deserialiseFromRawBytesHex (Api.AsSigningKey Api.AsStakeKey) "f5beaeff7932a4164d270afde7716067582412e8977e67986cd9b456fc082e3a" :: Api.SigningKey Api.StakeKey
      vote_stake_skey = voteSigningKeyFromStakeSigningKey stake_skey :: VoteSigningKey
  in sign makeVotePayloadCBOR vote_stake_skey

prettyVote :: IO ()
prettyVote = do
  let
    stake_skey = fromMaybe (error "BOOM") $ Api.deserialiseFromRawBytesHex (Api.AsSigningKey Api.AsStakeKey) "f5beaeff7932a4164d270afde7716067582412e8977e67986cd9b456fc082e3a" :: Api.SigningKey Api.StakeKey
    vote_stake_skey = voteSigningKeyFromStakeSigningKey stake_skey :: VoteSigningKey
    vote_pubkey = fromMaybe (error "BOOM") $ Api.deserialiseFromRawBytesHex Api.AsVotingKeyPublic "0036ef3e1f0d3f5989e2d155ea54bdb2a72c4c456ccb959af4c94868f473f5a0" :: Api.VotingKeyPublic
    rewards_addr = fromMaybe (error "BOOM") $ Api.deserialiseFromRawBytesHex Api.AsStakeAddress "e0ae3a0a7aeda4aea522e74e4fe36759fca80789a613a58a4364f6ecef" :: RewardsAddress
    slot' = 4
    payload     = mkVotePayload vote_pubkey (getVoteVerificationKey vote_stake_skey) rewards_addr slot'
    payloadCBOR = CBOR.serialize' payload
    vote@(Vote voteMeta _) = makeVoteReg
    VotePayload votePub stakePub rewardsAddr slot = voteMeta
  putStrLn $ "Vote public key used        (hex): " <> BSC.unpack (Api.serialiseToRawBytesHex votePub)
  putStrLn $ "Stake public key used       (hex): " <> BSC.unpack (Api.serialiseToRawBytesHex stakePub)
  putStrLn $ "Rewards address used        (hex): " <> BSC.unpack (Api.serialiseToRawBytesHex rewardsAddr)
  putStrLn $ "Slot registered                  : " <> show slot
  putStrLn $ "Vote registration signature (hex): " <> BSC.unpack (Base16.encode . Crypto.rawSerialiseSigDSIGN $ voteSignature vote)
  putStrLn $ "Raw CBOR payload            (hex): " <> BSC.unpack (Base16.encode makeVotePayloadCBOR)
  putStrLn $ "CBOR payload hashed         (hex): " <> BSC.unpack (Base16.encode makeVotePayloadHash)
  putStrLn $ "CBOR payload signature      (hex): " <> BSC.unpack (Base16.encode . Crypto.rawSerialiseSigDSIGN $ makeVotePayloadSig)
