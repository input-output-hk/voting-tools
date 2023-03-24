{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Catalyst.Registration.Types
  ( Delegations(..)
  , DelegationWeight
  , delegations
  , delegationsToTxMetadataValue
  , delegationsFromTxMetadataValue
  , createVoteRegistration
  , VotePayload(..)
  , mkVotePayload
  , signVotePayload
  , votePayloadFromTxMetadata
  , votePayloadToTxMetadata
  , Vote
  , voteRegistrationDelegations
  , voteRegistrationVerificationKey
  , voteRegistrationRewardsAddress
  , voteRegistrationStakeAddress
  , voteRegistrationSlot
  , voteRegistrationPurpose
  , voteRegistrationStakeHash
  , voteSignature
  , voteFromTxMetadata
  , voteToTxMetadata
  , VoteRewardsAddress(..)
  , metadataMetaKey
  , signatureMetaKey
  , componentPath
  , MetadataParsingError(..)
  , AsMetadataParsingError(..)
  , prettyPrintMetadataParsingError
  , prettyPrintMetadataPath
  , prettyPrintComponent
  , ParseRegistrationError(..)
  , AsParseRegistrationError(..)
  , parseRegistration
  , VoteRegistrationComponent(..)
  , module Purpose
  ) where

import           Cardano.Api (AsType (AsTxMetadata), HasTypeProxy (..), SerialiseAsCBOR (..),
                   TxMetadata (TxMetadata), TxMetadataValue (..), makeTransactionMetadata,
                   serialiseToRawBytes)
import           Cardano.Catalyst.Registration.Types.Purpose (Purpose, catalystPurpose)
import           Cardano.Ledger.Crypto (Crypto (..), StandardCrypto)
import           Control.Applicative ((<|>))
import           Control.Lens ((#))
import           Control.Lens.TH (makeClassyPrisms)
import           Control.Monad.Except (throwError)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import           Data.List (find)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Traversable (forM)
import           Data.Word (Word32, Word64)

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api
import qualified Cardano.Binary as CBOR
import qualified Cardano.Catalyst.Registration.Types.Purpose as Purpose
import qualified Cardano.Crypto.DSIGN as Crypto
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vector

import           Cardano.API.Extended (AsType (AsVotingKeyPublic), VotingKeyPublic (..))
import           Cardano.Catalyst.Crypto (AsType (AsStakeVerificationKey), StakeSigningKey,
                   StakeVerificationKey, getStakeVerificationKey, sign, stakeVerificationKeyHash,
                   verify)

data VoteRegistrationComponent
  = RegoDelegations
  | RegoStakeVerificationKey
  | RegoRewardsAddress
  | RegoSlotNum
  | RegoVotingPurpose
  | RegoSignature
  deriving (Eq, Show)

prettyPrintComponent :: VoteRegistrationComponent -> Text
prettyPrintComponent RegoDelegations
    = "delegations"
prettyPrintComponent RegoStakeVerificationKey
    = "stake verification key"
prettyPrintComponent RegoRewardsAddress
    = "rewards address"
prettyPrintComponent RegoSlotNum
    = "slot number"
prettyPrintComponent RegoVotingPurpose
    = "voting purpose"
prettyPrintComponent RegoSignature
    = "signature"

type MetadataPath = (Word64, Integer)

componentPath :: VoteRegistrationComponent -> MetadataPath
componentPath RegoDelegations          = (61284, 1)
componentPath RegoStakeVerificationKey = (61284, 2)
componentPath RegoRewardsAddress       = (61284, 3)
componentPath RegoSlotNum              = (61284, 4)
componentPath RegoVotingPurpose        = (61284, 5)
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

data VoteRewardsAddress =
    Address (Api.Address Api.ShelleyAddr)
  | RewardsAddress Api.StakeAddress -- This is the legacy Address type.
  deriving (Eq, Ord, Show)

instance Api.SerialiseAsRawBytes VoteRewardsAddress where
  serialiseToRawBytes (Address addr) =
    Api.serialiseToRawBytes addr
  serialiseToRawBytes (RewardsAddress stakeAddr) =
    Api.serialiseToRawBytes stakeAddr
  deserialiseFromRawBytes AsVoteRewardsAddress bs =
    fmap RewardsAddress (Api.deserialiseFromRawBytes Api.AsStakeAddress bs) <|>
    fmap Address (Api.deserialiseFromRawBytes (Api.AsAddress Api.AsShelleyAddr) bs)

instance HasTypeProxy VoteRewardsAddress where
  data AsType VoteRewardsAddress = AsVoteRewardsAddress
  proxyToAsType _ = AsVoteRewardsAddress

instance ToJSON VoteRewardsAddress where
  toJSON = Aeson.String . ("0x" <>) . T.decodeUtf8 . Api.serialiseToRawBytesHex

instance FromJSON VoteRewardsAddress where
  parseJSON = Aeson.withText "VoteRewardsAddress" $ \str -> case T.stripPrefix "0x" str of
    Nothing  -> fail "Missing hex identifier '0x'."
    Just hex ->
      case Api.deserialiseFromRawBytesHex AsVoteRewardsAddress $ T.encodeUtf8 hex of
        Nothing -> fail "Failed to decode rewards address."
        Just votePub -> pure votePub

-- | As of CIP-36, voting power can now be delegated between multiple voting
-- keys. 'DelegationWeight' represents the proportion of the voting power an
-- associated voting key should receive.
--
-- It is a 4-byte unsigned integer (CBOR major type 0). The value may range from
-- 0 to 2^32-1.
type DelegationWeight = Word32

data Delegations key
  = LegacyDelegation key
  | Delegations (NonEmpty (key, DelegationWeight))
  deriving (Eq, Ord, Show)


instance ToJSON key => ToJSON (Delegations key) where
  toJSON (LegacyDelegation key) = Aeson.toJSON key
  toJSON (Delegations ds)       = Aeson.toJSON ds

instance FromJSON key => FromJSON (Delegations key) where
  parseJSON v = parseDelegations v <|> parseLegacyDelegation v
    where
      parseDelegations :: Aeson.Value -> Aeson.Parser (Delegations key)
      parseDelegations = Aeson.withArray "Delegations" $ \ds ->
        case Vector.toList ds of
           [] -> fail "expected one or more delegations, got none"
           vals -> fmap (Delegations . NE.fromList) $ forM vals $
             Aeson.withArray "Delegation Elements" $ \keyWeight ->
               case Vector.toList keyWeight of
                 key:weight:[] ->
                   (,) <$> Aeson.parseJSON key <*> Aeson.parseJSON weight
                 _ ->
                   fail "expected a tuple of exactly two elements: (voting key, weight)"

      parseLegacyDelegation :: Aeson.Value -> Aeson.Parser (Delegations key)
      parseLegacyDelegation = fmap LegacyDelegation . Aeson.parseJSON

delegations :: Delegations key -> NonEmpty (key, DelegationWeight)
delegations (LegacyDelegation key) = pure (key, 1)
delegations (Delegations ds)       = ds

delegationsToTxMetadataValue
  :: Api.SerialiseAsRawBytes key
  => Delegations key
  -> Api.TxMetadataValue
delegationsToTxMetadataValue (LegacyDelegation key) =
  TxMetaBytes $ serialiseToRawBytes key
delegationsToTxMetadataValue (Delegations keyWeights) =
  TxMetaList
    $ fmap (\(key, weight) ->
              TxMetaList [ TxMetaBytes $ serialiseToRawBytes key
                         , TxMetaNumber $ fromIntegral weight
                         ]
           )
    $ NE.toList keyWeights

delegationsFromTxMetadataValue ::
  Api.TxMetadataValue
  -> Either MetadataParsingError (Delegations VotingKeyPublic)
delegationsFromTxMetadataValue (TxMetaBytes bytes) =
  case Api.deserialiseFromRawBytes AsVotingKeyPublic bytes of
    Nothing ->
      throwError
      $ _MetadataParseFailure
      # ( RegoDelegations
        , "unable to deserialise voting key from bytes"
        )
    Just votePub ->
      pure $ LegacyDelegation votePub
delegationsFromTxMetadataValue (TxMetaList [])   =
  throwError
  $ _MetadataParseFailure
  # ( RegoDelegations
    , "list of delegations was empty"
    )
delegationsFromTxMetadataValue (TxMetaList vals) =
  fmap (Delegations . NE.fromList) . forM vals $ \val ->
    case val of
      TxMetaList ((TxMetaBytes votePubBytes):(TxMetaNumber weight):[]) ->
        case Api.deserialiseFromRawBytes AsVotingKeyPublic votePubBytes of
          Nothing ->
            throwError
            $ _MetadataParseFailure
            # ( RegoDelegations
              , "unable to deserialise voting key from bytes"
              )
          Just votePub ->
            if weight > fromIntegral (maxBound :: Word32)
               || weight < fromIntegral (minBound :: Word32)
            then
              throwError
              $ _MetadataParseFailure
              # ( RegoDelegations
                , "delegation weight exceeded range from 0 to 2^32-1"
                )
            else
              pure (votePub, fromIntegral weight)
      _                              ->
        throwError
        $ _MetadataUnexpectedType
        # ( RegoDelegations
          , "list of exactly two elements [voting key, weight]"
          )
delegationsFromTxMetadataValue _ =
  throwError
  $ _MetadataUnexpectedType
  # (RegoDelegations, "bytes or list of (bytes, weight)")

-- | The payload of a vote (vote public key and stake verification
-- key).
data VotePayload = VotePayload
  { _votePayloadDelegations     :: Delegations VotingKeyPublic
  , _votePayloadVerificationKey :: StakeVerificationKey
  , _votePayloadRewardsAddr     :: VoteRewardsAddress
  , _votePayloadSlot            :: Integer
  , _votePayloadPurpose         :: Maybe Purpose
  }
  deriving (Eq, Ord, Show)

-- | The signed vote payload.
data Vote
  = Vote { _voteMeta :: VotePayload
         , _voteSig  :: Crypto.SigDSIGN (DSIGN StandardCrypto)
         }
  deriving (Eq, Show)

instance Ord Vote where
  compare (Vote a1 a2) (Vote b1 b2) =
    let
      a' = ( a1, CBOR.serialize' a2 )
      b' = ( b1, CBOR.serialize' b2 )
    in
      compare a' b'

voteRegistrationDelegations :: Vote -> Delegations VotingKeyPublic
voteRegistrationDelegations = _votePayloadDelegations . _voteMeta

voteRegistrationVerificationKey :: Vote -> StakeVerificationKey
voteRegistrationVerificationKey = _votePayloadVerificationKey . _voteMeta

voteRegistrationRewardsAddress :: Vote -> VoteRewardsAddress
voteRegistrationRewardsAddress = _votePayloadRewardsAddr . _voteMeta

voteRegistrationSlot :: Vote -> Integer
voteRegistrationSlot = _votePayloadSlot . _voteMeta

voteRegistrationPurpose :: Vote -> Maybe Purpose
voteRegistrationPurpose = _votePayloadPurpose . _voteMeta

voteRegistrationStakeHash :: Vote -> Api.Hash Api.StakeKey
voteRegistrationStakeHash = stakeVerificationKeyHash . voteRegistrationVerificationKey

voteRegistrationStakeAddress :: Api.NetworkId -> Vote -> Api.StakeAddress
voteRegistrationStakeAddress nw =
  Api.makeStakeAddress nw . Api.StakeCredentialByKey . voteRegistrationStakeHash

instance HasTypeProxy VotePayload where
  data AsType VotePayload = AsVotePayload
  proxyToAsType _ = AsVotePayload

instance SerialiseAsCBOR VotePayload where
  serialiseToCBOR =  serialiseToCBOR . votePayloadToTxMetadata
  deserialiseFromCBOR _ bs = do
    let
      changeError :: (a -> c) -> Either a b -> Either c b
      changeError = first

    meta <- deserialiseFromCBOR AsTxMetadata bs
    changeError (CBOR.DecoderErrorCustom "VotePayload" . prettyPrintMetadataParsingError)
      $ votePayloadFromTxMetadata meta

mkVotePayload
  :: Delegations VotingKeyPublic
  -- ^ Vote power delegations
  -> StakeVerificationKey
  -- ^ Vote verification key
  -> VoteRewardsAddress
  -- ^ Address used to pay for the vote registration
  -> Integer
  -- ^ Slot registration created at
  -> Maybe Purpose
  -- ^ Voting purpose (0 for Catalyst)
  -> VotePayload
  -- ^ Payload of the vote
mkVotePayload votepub vkey rewardsAddr slot purpose =
  VotePayload votepub vkey rewardsAddr slot purpose

signVotePayload
  :: VotePayload
  -- ^ Vote payload
  -> Crypto.SigDSIGN (DSIGN StandardCrypto)
  -- ^ Signature
  -> Maybe Vote
  -- ^ Signed vote
signVotePayload payload@(VotePayload { _votePayloadVerificationKey = vkey }) sig =
  let
    payloadCBOR = serialiseToCBOR payload
  in
    if verify vkey payloadCBOR sig == False
    then Nothing
    else Just $ Vote payload sig

votePayloadToTxMetadata :: VotePayload -> TxMetadata
votePayloadToTxMetadata (VotePayload ds stkVerify paymentAddr slot mVotingPurpose) =
  makeTransactionMetadata $ M.fromList [
    ( 61284, TxMetaMap $
      [ (TxMetaNumber 1, delegationsToTxMetadataValue ds)
      , (TxMetaNumber 2, TxMetaBytes $ serialiseToRawBytes stkVerify)
      , (TxMetaNumber 3, TxMetaBytes $ serialiseToRawBytes paymentAddr)
      , (TxMetaNumber 4, TxMetaNumber slot)
      ] ++ (case mVotingPurpose of
              Nothing -> []
              Just v  -> [(TxMetaNumber 5, Purpose.toTxMetadataValue v)])
    )
  ]

votePayloadFromTxMetadata :: TxMetadata -> Either MetadataParsingError VotePayload
votePayloadFromTxMetadata meta = do
  -- DECISION #09:
  --   We found some valid TxMetadata but we failed to find:

  -- DECISION #09A:
  --   the delegations under '61284' > '1'
  ds <-
    metaValue RegoDelegations meta >>= delegationsFromTxMetadataValue
  -- DECISION #09B:
  --   the stake verifiaction key under '61284' > '2'
  stkVerifyRaw   <- metaValue RegoStakeVerificationKey meta >>= asBytes RegoStakeVerificationKey
  -- DECISION #09C:
  --   the rewards address under '61284' > '3'
  voteRewardsAddrRaw <- metaValue RegoRewardsAddress meta >>= asBytes RegoRewardsAddress
  -- DECISION #09D:
  --   the slot number under '61284' > '4'
  slot           <- metaValue RegoSlotNum meta >>= asInt RegoSlotNum
  votingPurpose <-
    case metaValue RegoVotingPurpose meta >>= asInt RegoVotingPurpose of
      -- Ignore missing voting purpose (it's optional).
      Left (MetadataMissing _) ->
        pure Nothing
      -- Throw error if it's present but fails to parse as integer.
      Left e                   ->
        throwError e
      Right v | v > 0          ->
        throwError $ _MetadataParseFailure # (RegoVotingPurpose, "non-catalyst vote purpose")
      Right v | v == 0         ->
        pure $ Just catalystPurpose
      Right _                  ->
        throwError $ _MetadataParseFailure # (RegoVotingPurpose, "negative voting purpose")

  -- DECISION #10:
  --   We found a vote registration with all the correct parts, but were unable
  --   to:

  -- DECISION #10A:
  --   deserialise the stake verification key
  stkVerify <- case Api.deserialiseFromRawBytes AsStakeVerificationKey stkVerifyRaw of
    Nothing  -> throwError (_MetadataParseFailure # (RegoStakeVerificationKey, "Failed to deserialise."))
    Just x   -> pure x
  -- DECISION #10A:
  --   deserialise the rewards address
  voteRewardsAddr <- case Api.deserialiseFromRawBytes AsVoteRewardsAddress voteRewardsAddrRaw of
    Nothing -> throwError (_MetadataParseFailure # (RegoRewardsAddress, "Failed to deserialise. " <>  T.pack (show $ B16.encode voteRewardsAddrRaw)))
    Just x  -> pure x

  pure
    $ mkVotePayload ds stkVerify voteRewardsAddr slot votingPurpose

voteToTxMetadata :: Vote -> TxMetadata
voteToTxMetadata (Vote payload sig) =
  let
    payloadMeta = votePayloadToTxMetadata payload
    sigMeta = makeTransactionMetadata $ M.fromList [
        (signatureMetaKey, TxMetaMap [(TxMetaNumber 1, TxMetaBytes $ Crypto.rawSerialiseSigDSIGN sig)])
      ]
  in
    payloadMeta <> sigMeta

voteFromTxMetadata :: TxMetadata -> Either MetadataParsingError Vote
voteFromTxMetadata meta = do
  payload <- votePayloadFromTxMetadata meta

  -- DECISION #09:
  --   We found some valid TxMetadata but we failed to find:

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

  -- DECISION #11:
  --   We found and deserialised the vote registration but the vote registration
  --   signature is invalid.
  case payload `signVotePayload` sig of
    Nothing   -> throwError (_MetadataInvalidSignature # ())
    Just vote -> pure vote

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
                case find (\(k, _) -> k == TxMetaNumber k2) map2 of
                    Just (_, v) ->
                        pure v
                    Nothing ->
                        throwError (_MetadataMissing # comp)
            _ -> throwError (_MetadataMissing # comp)

asBytes :: VoteRegistrationComponent -> TxMetadataValue -> Either MetadataParsingError ByteString
asBytes _    (TxMetaBytes bs) = pure bs
asBytes comp _                = throwError (_MetadataUnexpectedType # (comp, "TxMetaBytes"))

asInt :: VoteRegistrationComponent -> TxMetadataValue -> Either MetadataParsingError Integer
asInt _ (TxMetaNumber int) = pure int
asInt comp _               = throwError (_MetadataUnexpectedType # (comp, "TxMetaNumber"))

voteSignature :: Vote -> Crypto.SigDSIGN Crypto.Ed25519DSIGN
voteSignature (Vote _ sig) = sig

metadataMetaKey :: Word64
metadataMetaKey = 61284

signatureMetaKey :: Word64
signatureMetaKey = 61285

-- | Create a vote registration payload.
createVoteRegistration
  :: StakeSigningKey
  -> Delegations VotingKeyPublic
  -> VoteRewardsAddress
  -> Integer
  -> Vote
createVoteRegistration skey ds rewardsAddr slot =
    let
      payload     =
        mkVotePayload
          ds
          (getStakeVerificationKey skey)
          rewardsAddr
          slot
          -- Catalyst voting purpose
          (Just catalystPurpose)
      payloadCBOR = serialiseToCBOR payload

      payloadSig  :: Crypto.SigDSIGN (DSIGN StandardCrypto)
      payloadSig  = payloadCBOR `sign` skey
  in
    fromMaybe (error "Failed to sign vote payload") $
      signVotePayload payload payloadSig

data ParseRegistrationError
  = ParseFailedToDecodeTxMetadata !Api.TxMetadataJsonError
  | ParseFailedToDecodeVoteRegistration !MetadataParsingError
  deriving (Eq, Show)

makeClassyPrisms ''ParseRegistrationError

parseRegistration
  :: Aeson.Value
  -> Either ParseRegistrationError Vote
parseRegistration rego = do
  voteRegoTxMetadata <-
    handleEither
    (\e -> _ParseFailedToDecodeTxMetadata # e)
    $ Api.metadataFromJson Api.TxMetadataJsonNoSchema rego

  voteRego <-
    handleEither (\e -> _ParseFailedToDecodeVoteRegistration # e)
    $ voteFromTxMetadata voteRegoTxMetadata

  pure voteRego

handleEither :: (e -> e') -> Either e x -> Either e' x
handleEither f = either (throwError . f) pure
