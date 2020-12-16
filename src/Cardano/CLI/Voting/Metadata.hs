{-# LANGUAGE FlexibleContexts #-}

-- | A vote in Voltaire is encoded as transaction metadata. We
-- distinguish two parts of the vote here: the payload, and the signed
-- vote. The payload consists of the vote public key, and the stake
-- verification key. The payload must be signed before it is
-- considered a valid vote.
module Cardano.CLI.Voting.Metadata ( VotePayload
                                   , Vote
                                   , mkVotePayload
                                   , signVotePayload
                                   , voteMetadata
                                   , voteSignature
                                   ) where

import           Cardano.API (SerialiseAsRawBytes, StakeKey, TxMetadata (TxMetadata),
                     VerificationKey, makeTransactionMetadata, serialiseToRawBytes)
import           Cardano.Api.Typed (TxMetadataValue (TxMetaBytes, TxMetaList, TxMetaMap, TxMetaNumber, TxMetaText))
import           Cardano.Binary (ToCBOR)
import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import           Cardano.Ledger.Crypto (ADDRHASH, Crypto, DSIGN, HASH, KES, VRF)
import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import qualified Shelley.Spec.Ledger.Keys as Shelley

import           Cardano.API.Extended (VotingKeyPublic)
import           Cardano.CLI.Voting.Signing (VoteSigningKey, verificationKeyRawBytes)

-- | The payload of a vote (vote public key and stake verification
-- key).
newtype VotePayload = VotePayload TxMetadata
  deriving (Eq, Show)

-- | The signed vote payload.
data Vote
  = Vote { _voteMeta :: TxMetadata
         , _voteSig  :: ByteString
         }
  deriving (Eq, Show)

instance ToCBOR TxMetadata where
  toCBOR (TxMetadata m) = CBOR.toCBOR m

instance ToCBOR TxMetadataValue where
  toCBOR (TxMetaNumber num) = CBOR.toCBOR num
  toCBOR (TxMetaBytes bs)   = CBOR.toCBOR bs
  toCBOR (TxMetaText txt)   = CBOR.toCBOR txt
  toCBOR (TxMetaList xs)    = CBOR.toCBOR xs
  -- Bit of a subtlety here. TxMetaMap is represented as a list of
  -- tuples, if we want to match the CBOR encoding of a traditional
  -- Map, we need to convert this list of tuples to a Map and then
  -- CBOR encode it. This means we may lose map entries if there are
  -- duplicate keys. I've decided this is OK as the promised interface
  -- is clearly a "Map".
  toCBOR (TxMetaMap m)      = CBOR.toCBOR (M.fromList m)

instance ToCBOR VotePayload where
  toCBOR (VotePayload meta) = CBOR.toCBOR meta

instance ToCBOR Vote where
  toCBOR = CBOR.toCBOR . voteMetadata

mkVotePayload
  :: VotingKeyPublic
  -- ^ Voting public key
  -> VoteSigningKey
  -- ^ Used for stake verification key
  -> VotePayload
  -- ^ Payload of the vote
mkVotePayload votepub voteSign =
  VotePayload
    $ makeTransactionMetadata
    $ M.fromList [ (61284, TxMetaMap
        [ (TxMetaNumber 1, TxMetaBytes $ serialiseToRawBytes votepub)
        , (TxMetaNumber 2, TxMetaBytes $ verificationKeyRawBytes voteSign)
        ])]

signVotePayload
  :: VotePayload
  -- ^ Vote payload
  -> Shelley.SignedDSIGN StandardCrypto a
  -- ^ Signature
  -> Vote
  -- ^ Signed vote
signVotePayload (VotePayload votePayload) (DSIGN.SignedDSIGN sig) =
  let
    sigBytes = Crypto.rawSerialiseSigDSIGN sig
  in
    Vote votePayload sigBytes

voteMetadata :: Vote -> TxMetadata
voteMetadata (Vote payloadMeta sigBytes) =
  let
    sigMeta = makeTransactionMetadata (M.fromList [
        (61285, TxMetaMap [(TxMetaNumber 1, TxMetaBytes sigBytes)])
      ])
  in
    payloadMeta <> sigMeta

voteSignature :: Vote -> ByteString
voteSignature (Vote _ sig) = sig
