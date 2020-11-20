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
                                   ) where

import Cardano.API (TxMetadata(TxMetadata), VerificationKey, StakeKey, makeTransactionMetadata, serialiseToRawBytes)
import Cardano.Api.Typed (TxMetadataValue(TxMetaNumber, TxMetaBytes, TxMetaText, TxMetaList, TxMetaMap))
import Cardano.Binary (ToCBOR)
import qualified Cardano.Binary as CBOR
import qualified Data.Map.Strict as M
import Data.ByteString (ByteString)

import Cardano.API.Voting (VotingKeyPublic)

-- | The payload of a vote (vote public key and stake verification
-- key).
newtype VotePayload = VotePayload TxMetadata
  deriving (Eq, Show)

-- | The signed vote payload.
newtype Vote        = Vote TxMetadata
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
  -- duplicate keys.
  toCBOR (TxMetaMap m)      = CBOR.toCBOR (M.fromList m)

instance ToCBOR VotePayload where
  toCBOR (VotePayload meta) = CBOR.toCBOR meta

instance ToCBOR Vote where
  toCBOR (Vote meta) = CBOR.toCBOR meta

mkVotePayload
  :: VotingKeyPublic
  -- ^ Voting public key
  -> VerificationKey StakeKey
  -- ^ Stake verification key
  -> VotePayload
  -- ^ Payload of the vote
mkVotePayload votepub stkVerify =
  VotePayload
    $ makeTransactionMetadata
    $ M.fromList [ (61284, TxMetaMap
        [ (TxMetaNumber 1, TxMetaBytes $ serialiseToRawBytes votepub)
        , (TxMetaNumber 2, TxMetaBytes $ serialiseToRawBytes stkVerify)
        ])]

signVotePayload
  :: VotePayload
  -- ^ Vote payload
  -> ByteString
  -- ^ Raw signature bytes
  -> Vote
  -- ^ Signed vote
signVotePayload (VotePayload votePayload) sig =
  let
    sigMetadata = makeTransactionMetadata (M.fromList [
        (61285, TxMetaMap [(TxMetaNumber 1, TxMetaBytes sig)])
      ])
  in
    Vote $ votePayload <> sigMetadata

voteMetadata :: Vote -> TxMetadata
voteMetadata (Vote meta) = meta
