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

import           Cardano.API (StakeKey, TxMetadata (TxMetadata), VerificationKey,
                     makeTransactionMetadata, serialiseToRawBytes)
import           Cardano.Api.Typed (TxMetadataValue (TxMetaBytes, TxMetaList, TxMetaMap, TxMetaNumber, TxMetaText))
import           Cardano.Binary (ToCBOR)
import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as M

import           Cardano.API.Extended (VotingKeyPublic)

-- | The payload of a vote (vote public key and stake verification
-- key).
newtype VotePayload
  = VotePayload { _votePayloadVoteKey :: VotingKeyPublic
                , _votePayloadVerificationKey :: VerificationKey StakeKey
                }
  deriving (Eq, Show)

-- | The signed vote payload.
data Vote alg
  = Vote { _voteMeta :: VotePayload
         , _voteSig  :: Crypto.SigDSIGN alg
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
  -> VerificationKey StakeKey
  -- ^ Stake verification key
  -> VotePayload
  -- ^ Payload of the vote
mkVotePayload votepub stkVerify = VotePayload votepub stkVerify

signVotePayload
  :: Crypto.DSIGNAlgorithm alg
  => VotePayload
  -- ^ Vote payload
  -> Crypto.SigDSIGN alg
  -- ^ Signature
  -> Either String Vote
  -- ^ Signed vote
signVotePayload payload@(VotePayload votePub (StakeVerificationKey (Shelley.VKey vkey))) sig =
  let
    payloadCBOR = CBOR.serialize' payload
  in
    case verifyDSIGN () vkey payloadCBOR sig of
      Left err -> Left err
      Right () -> Right $ Vote payload sig

toTxMetadata :: Vote -> TxMetadata
toTxMetadata (Vote payload sig) =
  let
    payloadMeta = makeTransactionMetadata $ M.fromList [ (61284, TxMetaMap
        [ (TxMetaNumber 1, TxMetaBytes $ serialiseToRawBytes votepub)
        , (TxMetaNumber 2, TxMetaBytes $ serialiseToRawBytes stkVerify)
        ])]
    sigMeta = makeTransactionMetadata $ M.fromList [
        (61285, TxMetaMap [(TxMetaNumber 1, TxMetaBytes $ Crytpo.rawSerialiseSigDSIGN sig)])
      ]
  in
    payloadMeta <> sigMeta

parseMetadataFromJson :: Aeson.Value -> Either String Api.TxMetadata
parseMetadataFromJson = Api.metadataFromJson Api.TxMetadataJsonDetailedSchema

fromTxMetadata :: TxMetadata -> Either MetadataParsingError Vote
fromTxMetadata (TxMetadata map) = do
  votePubRaw   <- metaKey 61284 >>= metaNum 1 >>= asBytes
  stkVerifyRaw <- metaKey 61284 >>= metaNum 2 >>= asBytes
  sigBytes     <- metaKey 61285 >>= metaNum 1 >>= asBytes
  
  sig       <- case Crypto.rawDeserialiseSigDSIGN sigBytes of
    Left err -> throwError (_DeserialiseSigDSIGNFailure (err, sigBytes))
    Right x  -> pure x
  stkVerify <- case deserialiseFromRawBytes stkVerifyRaw of
    Left err -> throwError (_DeserialiseVerKeyDSIGNFailure (err, stkVerifyRaw))
    Right x  -> pure x
  votePub   <- case deserialiseFromRawBytes votepub of
    Left err -> throwError (_DeserialiseVotePublicKeyFailure (err, votePubRaw))
    Right x  -> pure x

  case (mkVotePayload votePub stkVerify) `signVotePayload` sig of
    Left err   -> throwError (_MetadataSignatureInvalid # (err, sig))
    Right vote -> pure vote

  where
    metaKey :: Word64 -> TxMetadata -> Either MetadataParsingError TxMetdataValue
    metaKey key val@(TxMetadata map) =
      case M.lookup key map of
        Nothing     -> throwError (_MetadataMissingField # (val, key))
        Just x      -> pure x

    metaNum :: Integer -> TxMetadataValue -> Either MetadataParsingError TxMetadataValue
    metaNum key val@(TxMetaMap xs) =
      case find (\(k, v) -> k == TxMetaNumber key) xs of
        Nothing     -> throwError (_MetadataValueMissingField # (val, key))
        Just (_, x) -> pure x
    metaNum key (x)            = throwError (_MetadataValueUnexpectedType # ("TxMetaMap", x))

    asBytes :: TxMetadataValue -> Either MetadataParsingError ByteString
    asBytes (TxMetaBytes bs) = pure bs
    asBytes (x)              = throwError (_MetadataValueUnexpectedType # ("TxMetaBytes", x))

voteSignature :: Vote alg -> Crypto.DSIGN alg
voteSignature (Vote _ sig) = sig

metadataMetaKey :: _
metadataMetaKey = 61284

signatureMetaKey :: _
signatureMetaKey = 61285

-- | The database JSON has the meta key stored separately to the meta
--   value, use this function to combine them.
withMetaKey :: Word64 -> Aeson.Value -> Aeson.Object
withMetaKey metaKey val = HM.fromList [(T.show metaKey, val)]
