{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Voting.Signing ( VoteSigningKey
                                  , VoteVerificationKey
                                  , AsType(AsVoteVerificationKey)
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

import           Cardano.API (AsType (AsSigningKey, AsStakeExtendedKey, AsStakeKey, AsVerificationKey), FromSomeType,
                     HasTypeProxy, Key, SerialiseAsRawBytes(serialiseToRawBytes, deserialiseFromRawBytes), SigningKey, StakeExtendedKey,
                     StakeKey, VerificationKey, getVerificationKey, proxyToAsType,
                     serialiseToRawBytes, castVerificationKey)
import           Cardano.API.Extended (AsFileError, AsInputDecodeError, readSigningKeyFileAnyOf)
import           Cardano.Api.Typed (FromSomeType (FromSomeType))
import           Cardano.Api.Typed (ShelleySigningKey,
                     ShelleyWitnessSigningKey (WitnessStakeExtendedKey, WitnessStakeKey),
                     SigningKey (StakeExtendedSigningKey, StakeSigningKey), VerificationKey(StakeVerificationKey),
                     getShelleyKeyWitnessVerificationKey, makeShelleySignature,
                     toShelleySigningKey)
import           Cardano.CLI.Types (SigningKeyFile)
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Util as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import qualified Shelley.Spec.Ledger.Keys as Shelley

data VoteSigningKey
  = AStakeSigningKey         (SigningKey StakeKey)
  | AStakeExtendedSigningKey (SigningKey StakeExtendedKey)
  deriving Show

data VoteVerificationKey
  = AStakeVerificationKey (VerificationKey StakeKey)
  | AStakeExtendedVerificationKey (VerificationKey StakeExtendedKey)
  deriving (Eq, Show)

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

verificationKeyRawBytes :: VoteSigningKey -> ByteString
verificationKeyRawBytes (AStakeSigningKey k)         = serialiseToRawBytes $ getVerificationKey k
verificationKeyRawBytes (AStakeExtendedSigningKey k) = serialiseToRawBytes $ getVerificationKey k

sign
  :: Crypto.SignableRepresentation tosign
  => tosign
  -> VoteSigningKey
  -> Shelley.SignedDSIGN StandardCrypto tosign
sign payload vsk =
  withVoteShelleySigningKey vsk $ \skey ->
    payload `makeShelleySignature` skey

verify
  :: Crypto.SignableRepresentation tosign
  => VoteVerificationKey
  -> tosign
  -> Shelley.SignedDSIGN StandardCrypto tosign
  -> Bool
verify vkey payload sig =
  withVoteVerificationKey vkey $ \(StakeVerificationKey v) ->
    Shelley.verifySignedDSIGN v payload sig

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
