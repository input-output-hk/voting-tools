{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Cardano.CLI.Voting.Signing ( VoteSigningKey
                                  , VoteVerificationKey
                                  , VoteVerificationKeyHash
                                  , AsType(AsVoteVerificationKey, AsVoteVerificationKeyHash)
                                  , getVoteVerificationKeyHash
                                  , getVoteVerificationKey
                                  , withVoteVerificationKey
                                  , withVoteSigningKey
                                  , withVoteShelleySigningKey
                                  , sign
                                  , verify
                                  , readVoteSigningKeyFile
                                  , verificationKeyRawBytes
                                  ) where

import           Control.Monad.Except (MonadError)
import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString (ByteString)

import           Cardano.API
                     (StakeAddress, makeStakeAddress, Hash, AsType (AsHash, AsSigningKey, AsStakeExtendedKey, AsStakeKey, AsVerificationKey),
                     FromSomeType, HasTypeProxy, Key,
                     SerialiseAsRawBytes (deserialiseFromRawBytes, serialiseToRawBytes),
                     SigningKey, StakeExtendedKey, StakeKey, NetworkId, VerificationKey, castVerificationKey, verificationKeyHash,
                     getVerificationKey, proxyToAsType, serialiseToRawBytes)
import           Cardano.API.Extended (AsFileError, AsInputDecodeError, readSigningKeyFileAnyOf)
import           Cardano.Api.Typed (StakeCredential(StakeCredentialByKey), FromSomeType (FromSomeType), ShelleySigningKey,
                     ShelleyWitnessSigningKey (WitnessStakeExtendedKey, WitnessStakeKey),
                     SigningKey (StakeExtendedSigningKey, StakeSigningKey),
                     VerificationKey (StakeVerificationKey), getShelleyKeyWitnessVerificationKey,
                     makeShelleySignature, toShelleySigningKey)
import           Cardano.CLI.Types (SigningKeyFile)
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Util as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import qualified Shelley.Spec.Ledger.Keys as Shelley

import           Cardano.Ledger.Crypto (Crypto (..))

data VoteSigningKey
  = AStakeSigningKey         (SigningKey StakeKey)
  | AStakeExtendedSigningKey (SigningKey StakeExtendedKey)
  deriving Show

data VoteVerificationKey
  = AStakeVerificationKey (VerificationKey StakeKey)
  | AStakeExtendedVerificationKey (VerificationKey StakeExtendedKey)
  deriving (Eq, Show)

data VoteVerificationKeyHash
  = AStakeVerificationKeyHash (Hash StakeKey)
  | AStakeExtendedVerificationKeyHash (Hash StakeExtendedKey)
  deriving (Ord, Eq, Show)

instance HasTypeProxy VoteVerificationKeyHash where
  data AsType VoteVerificationKeyHash = AsVoteVerificationKeyHash
  proxyToAsType _ = AsVoteVerificationKeyHash

instance SerialiseAsRawBytes VoteVerificationKeyHash where
    serialiseToRawBytes (AStakeVerificationKeyHash h)         = serialiseToRawBytes h
    serialiseToRawBytes (AStakeExtendedVerificationKeyHash h) = serialiseToRawBytes h

    deserialiseFromRawBytes AsVoteVerificationKeyHash bs = do
      case AStakeExtendedVerificationKeyHash <$> deserialiseFromRawBytes (AsHash AsStakeExtendedKey) bs of
        Nothing -> AStakeVerificationKeyHash <$> deserialiseFromRawBytes (AsHash AsStakeKey) bs
        Just h  -> pure h

getVoteVerificationKeyHash :: VoteVerificationKey -> VoteVerificationKeyHash
getVoteVerificationKeyHash (AStakeVerificationKey k)         = AStakeVerificationKeyHash $ verificationKeyHash k
getVoteVerificationKeyHash (AStakeExtendedVerificationKey k) = AStakeExtendedVerificationKeyHash $ verificationKeyHash k

getVoteVerificationKey :: VoteSigningKey -> VoteVerificationKey
getVoteVerificationKey (AStakeSigningKey skey)         = AStakeVerificationKey         $ getVerificationKey skey
getVoteVerificationKey (AStakeExtendedSigningKey skey) = AStakeExtendedVerificationKey $ getVerificationKey skey

withVoteVerificationKey :: VoteVerificationKey -> (VerificationKey StakeKey -> a) -> a
withVoteVerificationKey ver f =
  let
    vkey = case ver of
      (AStakeVerificationKey vkey)         -> vkey
      (AStakeExtendedVerificationKey vkey) -> castVerificationKey vkey
  in
    f vkey

withStakeKeyHash :: VoteVerificationKeyHash -> (Hash StakeKey -> a) -> a
withStakeKeyHash ver f = withVoteVerificationKey ver $ \vkey ->
  let
    hash :: Hash StakeKey
    hash = verificationKeyHash vkey
  in
    f hash

toStakeAddr :: NetworkId -> Hash StakeKey -> StakeAddress
toStakeAddr nw = makeStakeAddress nw . StakeCredentialByKey

verificationKeyRawBytes :: VoteSigningKey -> ByteString
verificationKeyRawBytes (AStakeSigningKey k)         = serialiseToRawBytes $ getVerificationKey k
verificationKeyRawBytes (AStakeExtendedSigningKey k) = serialiseToRawBytes $ getVerificationKey k

sign
  :: Crypto.SignableRepresentation tosign
  => tosign
  -> VoteSigningKey
  -> Crypto.SigDSIGN (DSIGN StandardCrypto)
sign payload vsk =
  withVoteShelleySigningKey vsk $ \skey ->
    let
      (Crypto.SignedDSIGN sig) = payload `makeShelleySignature` skey
    in sig

verify
  :: Crypto.SignableRepresentation tosign
  => VoteVerificationKey
  -> tosign
  -> Crypto.SigDSIGN (DSIGN StandardCrypto)
  -> Bool
verify vkey payload sig =
  withVoteVerificationKey vkey $ \(StakeVerificationKey (Shelley.VKey v)) ->
    either (const False) (const True) $ Crypto.verifyDSIGN () v payload sig

withVoteSigningKey :: VoteSigningKey
                   -> (forall keyrole. Key keyrole => SigningKey keyrole -> a)
                   -> a
withVoteSigningKey vsk f =
  case vsk of
    AStakeSigningKey sk         -> f sk
    AStakeExtendedSigningKey sk -> f sk

withVoteShelleySigningKey :: VoteSigningKey -> (ShelleySigningKey -> a) -> a
withVoteShelleySigningKey vsk f =
  case vsk of
    AStakeSigningKey (StakeSigningKey dsign)                -> f ( toShelleySigningKey $ WitnessStakeKey (StakeSigningKey dsign))
    AStakeExtendedSigningKey (StakeExtendedSigningKey xprv) -> f ( toShelleySigningKey $ WitnessStakeExtendedKey (StakeExtendedSigningKey xprv))

readVoteSigningKeyFile
  :: ( MonadIO m
     , MonadError e m
     , AsFileError e fileErr
     , AsInputDecodeError fileErr
     )
  => SigningKeyFile
  -> m VoteSigningKey
readVoteSigningKeyFile skFile =
  readSigningKeyFileAnyOf bech32FileTypes textEnvFileTypes skFile

  where
    textEnvFileTypes =
      [ FromSomeType (AsSigningKey AsStakeKey)
                      AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                      AStakeExtendedSigningKey
      ]

    bech32FileTypes =
      [ FromSomeType (AsSigningKey AsStakeKey)
                      AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                      AStakeExtendedSigningKey
      ]

instance HasTypeProxy VoteVerificationKey where
  data AsType VoteVerificationKey = AsVoteVerificationKey
  proxyToAsType _ = AsVoteVerificationKey

instance SerialiseAsRawBytes VoteVerificationKey where
  serialiseToRawBytes (AStakeVerificationKey vkey)         = serialiseToRawBytes vkey
  serialiseToRawBytes (AStakeExtendedVerificationKey vkey) = serialiseToRawBytes vkey

  deserialiseFromRawBytes AsVoteVerificationKey bs =
    case (AStakeExtendedVerificationKey <$> deserialiseFromRawBytes (AsVerificationKey AsStakeExtendedKey) bs) of
      Nothing -> (AStakeVerificationKey <$> deserialiseFromRawBytes (AsVerificationKey AsStakeKey) bs)
      x       -> x
