{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A vote in Voltaire is encoded as transaction metadata. We
-- distinguish two parts of the vote here: the payload, and the signed
-- vote. The payload consists of the vote public key, and the stake
-- verification key. The payload must be signed before it is
-- considered a valid vote.
module Cardano.CLI.Voting.Metadata ( VotePayload(..)
                                   , Vote(..)
                                   , RewardsAddress
                                   , mkVotePayload
                                   , signVotePayload
                                   , voteToTxMetadata
                                   , voteSignature
                                   , MetadataParsingError(..)
                                   , AsMetadataParsingError(..)
                                   , voteFromTxMetadata
                                   , withMetaKey
                                   , metadataMetaKey
                                   , signatureMetaKey
                                   , voteRegistrationPublicKey
                                   , voteRegistrationVerificationKey
                                   , voteRegistrationRewardsAddress
                                   , voteRegistrationSlot
                                   , metadataToJson
                                   , parseMetadataFromJson
                                   , componentPath
                                   , prettyPrintMetadataParsingError
                                   , prettyPrintMetadataPath
                                   , prettyPrintComponent
                                   ) where

import           Cardano.Api (StakeKey, TxMetadata (TxMetadata), VerificationKey,
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
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word64)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import qualified Shelley.Spec.Ledger.Keys as Shelley

import           Cardano.API.Extended (AsType (AsVotingKeyPublic), VotingKeyPublic)
import           Cardano.CLI.Voting.Signing (AsType (AsVoteVerificationKey), VoteVerificationKey,
                     verify)

type RewardsAddress = Api.StakeAddress

-- | The payload of a vote (vote public key and stake verification
-- key).
data VotePayload
  = VotePayload { _votePayloadVoteKey         :: VotingKeyPublic
                , _votePayloadVerificationKey :: VoteVerificationKey
                , _votePayloadRewardsAddr     :: RewardsAddress
                , _votePayloadSlot            :: Integer
                }
  deriving (Eq, Show)

-- | The signed vote payload.
data Vote
  = Vote { _voteMeta :: VotePayload
         , _voteSig  :: Crypto.SigDSIGN (DSIGN StandardCrypto)
         }
  deriving (Eq, Show)

voteRegistrationPublicKey :: Vote -> VotingKeyPublic
voteRegistrationPublicKey = _votePayloadVoteKey . _voteMeta

voteRegistrationVerificationKey :: Vote -> VoteVerificationKey
voteRegistrationVerificationKey = _votePayloadVerificationKey . _voteMeta

voteRegistrationRewardsAddress :: Vote -> RewardsAddress
voteRegistrationRewardsAddress = _votePayloadRewardsAddr . _voteMeta

voteRegistrationSlot :: Vote -> Integer
voteRegistrationSlot = _votePayloadSlot . _voteMeta

data VoteRegistrationComponent
  = RegoVotingKey
  | RegoStakeVerificationKey
  | RegoRewardsAddress
  | RegoSlotNum
  | RegoSignature
  deriving (Eq, Show)

prettyPrintComponent RegoVotingKey
    = "voting key"
prettyPrintComponent RegoStakeVerificationKey
    = "stake verification key"
prettyPrintComponent RegoRewardsAddress
    = "rewards address"
prettyPrintComponent RegoSlotNum
    = "slot number"
prettyPrintComponent RegoSignature
    = "signature"

type MetadataPath = (Word64, Integer)

componentPath :: VoteRegistrationComponent -> MetadataPath
componentPath RegoVotingKey            = (61284, 1)
componentPath RegoStakeVerificationKey = (61284, 2)
componentPath RegoRewardsAddress       = (61284, 3)
componentPath RegoSlotNum              = (61284, 4)
componentPath RegoSignature            = (61285, 1)

prettyPrintMetadataPath :: MetadataPath -> Text
prettyPrintMetadataPath (k1, k2) = T.pack $ show k1 <> " > " <> show k2

data MetadataParsingError
  = MetadataMissing VoteRegistrationComponent
  | MetadataParseFailure VoteRegistrationComponent Text
  | MetadataUnexpectedType VoteRegistrationComponent Text
  | MetadataInvalidSignature
  deriving (Eq, Show)

prettyPrintMetadataParsingError :: MetadataParsingError -> Text
prettyPrintMetadataParsingError (MetadataMissing comp)
    = "The " <> prettyPrintComponent comp
    <> " could not be found at the metadata path: "
    <> prettyPrintMetadataPath (componentPath comp)
prettyPrintMetadataParsingError (MetadataParseFailure comp failure)
    = "The " <> prettyPrintComponent comp
    <> " at metadata path: " <> prettyPrintMetadataPath (componentPath comp)
    <> " failed to parse. Error was: " <> failure
prettyPrintMetadataParsingError (MetadataUnexpectedType comp expected)
    = "The " <> prettyPrintComponent comp
    <> " expected type '" <> expected <> "'"
    <> " at path " <> prettyPrintMetadataPath (componentPath comp)
prettyPrintMetadataParsingError MetadataInvalidSignature =
    "Signature validation failed"

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
  -> RewardsAddress
  -- ^ Address used to pay for the vote registration
  -> Integer
  -- ^ Slot registration created at
  -> VotePayload
  -- ^ Payload of the vote
mkVotePayload votepub vkey rewardsAddr slot = VotePayload votepub vkey rewardsAddr slot

signVotePayload
  :: VotePayload
  -- ^ Vote payload
  -> Crypto.SigDSIGN (DSIGN StandardCrypto)
  -- ^ Signature
  -> Maybe Vote
  -- ^ Signed vote
signVotePayload payload@(VotePayload { _votePayloadVerificationKey = vkey }) sig =
  let
    payloadCBOR = CBOR.serialize' payload
  in
    if verify vkey payloadCBOR sig == False
    then Nothing
    else Just $ Vote payload sig

votePayloadToTxMetadata :: VotePayload -> TxMetadata
votePayloadToTxMetadata (VotePayload votepub stkVerify paymentAddr slot) =
  makeTransactionMetadata $ M.fromList [ (61284, TxMetaMap
    [ (TxMetaNumber 1, TxMetaBytes $ serialiseToRawBytes votepub)
    , (TxMetaNumber 2, TxMetaBytes $ serialiseToRawBytes stkVerify)
    , (TxMetaNumber 3, TxMetaBytes $ serialiseToRawBytes paymentAddr)
    , (TxMetaNumber 4, TxMetaNumber slot)
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

parseMetadataFromJson :: Aeson.Value -> Either Api.TxMetadataJsonError Api.TxMetadata
parseMetadataFromJson = Api.metadataFromJson Api.TxMetadataJsonNoSchema

metadataToJson :: TxMetadata -> Aeson.Value
metadataToJson = Api.metadataToJson Api.TxMetadataJsonNoSchema

voteFromTxMetadata :: TxMetadata -> Either MetadataParsingError Vote
voteFromTxMetadata meta = do
  -- DECISION #09:
  --   We found some valid TxMetadata but we failed to find:

  -- DECISION #09A:
  --   the voting public key under '61284' > '1'
  votePubRaw     <- metaValue RegoVotingKey meta >>= asBytes RegoVotingKey
  -- DECISION #09B:
  --   the stake verifiaction key under '61284' > '2'
  stkVerifyRaw   <- metaValue RegoStakeVerificationKey meta >>= asBytes RegoStakeVerificationKey
  -- DECISION #09C:
  --   the rewards address under '61284' > '3'
  rewardsAddrRaw <- metaValue RegoRewardsAddress meta >>= asBytes RegoRewardsAddress
  -- DECISION #09D:
  --   the slot number under '61284' > '4'
  slot           <- metaValue RegoSlotNum meta >>= asInt RegoSlotNum
  -- DECISION #09E:
  --   the signature under '61285' > '1'
  sigBytes       <- metaValue RegoSignature meta >>= asBytes RegoSignature

  -- DECISION #10:
  --   We found a vote registration with all the correct parts, but were unable
  --   to:

  -- DECISION #10A:
  --   deserialise the signature
  sig       <- case Crypto.rawDeserialiseSigDSIGN sigBytes of
    Nothing -> throwError (_MetadataParseFailure # (RegoSignature, "Failed to deserialise."))
    Just x  -> pure x
  -- DECISION #10A:
  --   deserialise the stake verification key
  stkVerify <- case Api.deserialiseFromRawBytes AsVoteVerificationKey stkVerifyRaw of
    Nothing  -> throwError (_MetadataParseFailure # (RegoStakeVerificationKey, "Failed to deserialise."))
    Just x   -> pure x
  -- DECISION #10A:
  --   deserialise the voting public key
  votePub   <- case Api.deserialiseFromRawBytes AsVotingKeyPublic votePubRaw of
    Nothing -> throwError (_MetadataParseFailure # (RegoVotingKey, "Failed to deserialise."))
    Just x  -> pure x
  -- DECISION #10A:
  --   deserialise the rewards address
  rewardsAddr <- case Api.deserialiseFromRawBytes Api.AsStakeAddress rewardsAddrRaw of
    Nothing -> throwError (_MetadataParseFailure # (RegoRewardsAddress, "Failed to deserialise."))
    Just x  -> pure x

  let
    payload = mkVotePayload votePub stkVerify rewardsAddr slot

  -- DECISION #11:
  --   We found and deserialised the vote registration but the vote registration
  --   signature is invalid.
  case payload `signVotePayload` sig of
    Nothing   -> throwError (_MetadataInvalidSignature # ())
    Just vote -> pure vote

  where
    metaValue
        :: VoteRegistrationComponent
        -> TxMetadata
        -> Either MetadataParsingError TxMetadataValue
    metaValue comp (TxMetadata map1) =
        let
            (k1, k2) = componentPath comp
        in
            case M.lookup k1 map1 of
                Just (TxMetaMap map2) ->
                    case find (\(k, v) -> k == TxMetaNumber k2) map2 of
                        Just (_, v) ->
                            pure v
                        Nothing ->
                            throwError (_MetadataMissing # comp)
                _ -> throwError (_MetadataMissing # comp)

    asBytes :: VoteRegistrationComponent -> TxMetadataValue -> Either MetadataParsingError ByteString
    asBytes _    (TxMetaBytes bs) = pure bs
    asBytes comp x                = throwError (_MetadataUnexpectedType # (comp, "TxMetaBytes"))

    asInt :: VoteRegistrationComponent -> TxMetadataValue -> Either MetadataParsingError Integer
    asInt comp (TxMetaNumber int) = pure int
    asInt comp x                  = throwError (_MetadataUnexpectedType # (comp, "TxMetaNumber"))

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
