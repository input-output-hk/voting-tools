{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A vote in Voltaire is encoded as transaction metadata. We
-- distinguish two parts of the vote here: the payload, and the signed
-- vote. The payload consists of the vote public key, and the stake
-- verification key. The payload must be signed before it is
-- considered a valid vote.
module Cardano.CLI.Voting.Metadata ( VotePayload
                                   , Vote
                                   , mkVotePayload
                                   , signVotePayload
                                   , voteToTxMetadata
                                   , votePayloadToTxMetadata
                                   , voteSignature
                                   ) where

import           Cardano.API (StakeKey, TxMetadata (TxMetadata), VerificationKey,
                     makeTransactionMetadata, serialiseToRawBytes)
import           Cardano.Api.Typed (TxMetadata,
                     TxMetadataValue (TxMetaBytes, TxMetaList, TxMetaMap, TxMetaNumber, TxMetaText),
                     VerificationKey (StakeVerificationKey))
import qualified Cardano.Api.Typed as Api
import           Cardano.Binary (ToCBOR)
import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.DSIGN as Crypto
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Util as Crypto
import           Cardano.Ledger.Crypto (Crypto (..))
import           Control.Lens (( # ))
import           Control.Lens.TH (makeClassyPrisms)
import           Control.Monad.Except (throwError)
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import           Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Word (Word64)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import qualified Shelley.Spec.Ledger.Keys as Shelley

import           Cardano.API.Extended (AsType (AsVotingKeyPublic), VotingKeyPublic)
import           Cardano.CLI.Voting.Signing (AsType (AsVoteVerificationKey), VoteVerificationKey,
                     verify)

-- | The payload of a vote (vote public key and stake verification
-- key).
data VotePayload
  = VotePayload { _votePayloadVoteKey :: VotingKeyPublic
                , _votePayloadVerificationKey :: VoteVerificationKey
                }
  deriving (Eq, Show)

-- | The signed vote payload.
data Vote
  = Vote { _voteMeta :: VotePayload
         , _voteSig  :: Crypto.SigDSIGN (DSIGN StandardCrypto)
         }

data MetadataParsingError
  = MetadataMissingField TxMetadata Word64
  | MetadataValueMissingField TxMetadataValue Integer
  | MetadataValueUnexpectedType String TxMetadataValue
  | DeserialiseSigDSIGNFailure ByteString
  | DeserialiseVerKeyDSIGNFailure ByteString
  | DeserialiseVotePublicKeyFailure ByteString
  | MetadataSignatureInvalid VotePayload (Crypto.SigDSIGN (DSIGN StandardCrypto))

makeClassyPrisms ''MetadataParsingError

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
  toCBOR = CBOR.toCBOR . votePayloadToTxMetadata

instance ToCBOR Vote where
  toCBOR = CBOR.toCBOR . voteToTxMetadata


mkVotePayload
  :: VotingKeyPublic
  -- ^ Voting public key
  -> VoteVerificationKey
  -- ^ Vote verification key
  -> VotePayload
  -- ^ Payload of the vote
mkVotePayload votepub vkey = VotePayload votepub vkey

signVotePayload
  :: VotePayload
  -- ^ Vote payload
  -> Crypto.SigDSIGN (DSIGN StandardCrypto)
  -- ^ Signature
  -> Maybe Vote
  -- ^ Signed vote
signVotePayload payload@(VotePayload votePub vkey) sig =
  let
    payloadCBOR = CBOR.serialize' payload
  in
    if verify vkey payloadCBOR sig == False
    then Nothing
    else Just $ Vote payload sig

votePayloadToTxMetadata :: VotePayload -> TxMetadata
votePayloadToTxMetadata (VotePayload votepub stkVerify) =
  makeTransactionMetadata $ M.fromList [ (61284, TxMetaMap
    [ (TxMetaNumber 1, TxMetaBytes $ serialiseToRawBytes votepub)
    , (TxMetaNumber 2, TxMetaBytes $ serialiseToRawBytes stkVerify)
    ])]

voteToTxMetadata :: Vote -> TxMetadata
voteToTxMetadata (Vote payload sig) =
  let
    payloadMeta = votePayloadToTxMetadata payload
    sigMeta = makeTransactionMetadata $ M.fromList [
        (61285, TxMetaMap [(TxMetaNumber 1, TxMetaBytes $ Crypto.rawSerialiseSigDSIGN sig)])
      ]
  in
    payloadMeta <> sigMeta

parseMetadataFromJson :: Aeson.Value -> Either Api.TxMetadataJsonError TxMetadata
parseMetadataFromJson = Api.metadataFromJson Api.TxMetadataJsonDetailedSchema

fromTxMetadata :: TxMetadata -> Either MetadataParsingError Vote
fromTxMetadata meta = do
  votePubRaw   <- metaKey 61284 meta >>= metaNum 1 >>= asBytes
  stkVerifyRaw <- metaKey 61284 meta >>= metaNum 2 >>= asBytes
  sigBytes     <- metaKey 61285 meta >>= metaNum 1 >>= asBytes

  sig       <- case Crypto.rawDeserialiseSigDSIGN sigBytes of
    Nothing -> throwError (_DeserialiseSigDSIGNFailure # sigBytes)
    Just x  -> pure x
  stkVerify <- case Api.deserialiseFromRawBytes AsVoteVerificationKey stkVerifyRaw of
    Nothing  -> throwError (_DeserialiseVerKeyDSIGNFailure # stkVerifyRaw)
    Just x   -> pure x
  votePub   <- case Api.deserialiseFromRawBytes AsVotingKeyPublic votePubRaw of
    Nothing -> throwError (_DeserialiseVotePublicKeyFailure # votePubRaw)
    Just x  -> pure x

  let
    payload = mkVotePayload votePub stkVerify

  case payload `signVotePayload` sig of
    Nothing   -> throwError (_MetadataSignatureInvalid # (payload, sig))
    Just vote -> pure vote

  where
    metaKey :: Word64 -> TxMetadata -> Either MetadataParsingError TxMetadataValue
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

voteSignature :: Vote -> Crypto.SigDSIGN Crypto.Ed25519DSIGN
voteSignature (Vote _ sig) = sig

metadataMetaKey :: Integer
metadataMetaKey = 61284

signatureMetaKey :: Integer
signatureMetaKey = 61285

-- | The database JSON has the meta key stored separately to the meta
--   value, use this function to combine them.
withMetaKey :: Word64 -> Aeson.Value -> Aeson.Object
withMetaKey metaKey val = HM.fromList [(T.pack . show $ metaKey, val)]
