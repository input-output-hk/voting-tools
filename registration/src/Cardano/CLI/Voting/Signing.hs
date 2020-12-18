{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.Voting.Signing ( VoteSigningKey
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

import           Cardano.API (AsType (AsSigningKey, AsStakeExtendedKey, AsStakeKey), FromSomeType,
                     HasTypeProxy, Key, SerialiseAsRawBytes, SigningKey, StakeExtendedKey,
                     StakeKey, VerificationKey, getVerificationKey, proxyToAsType,
                     serialiseToRawBytes)
import           Cardano.API.Extended (AsFileError, AsInputDecodeError, readSigningKeyFileAnyOf)
import           Cardano.Api.Typed (FromSomeType (FromSomeType))
import           Cardano.Api.Typed (ShelleySigningKey,
                     ShelleyWitnessSigningKey (WitnessStakeExtendedKey, WitnessStakeKey),
                     SigningKey (StakeExtendedSigningKey, StakeSigningKey),
                     getShelleyKeyWitnessVerificationKey, makeShelleySignature,
                     toShelleySigningKey)
import           Cardano.CLI.Types (SigningKeyFile)
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Util as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import qualified Shelley.Spec.Ledger.Keys as Shelley

data VoteSigningKey = AStakeSigningKey         (SigningKey StakeKey)
                    | AStakeExtendedSigningKey (SigningKey StakeExtendedKey)
  deriving Show

-- data VoteVerificationKey
--   = AStakeVerificationKey

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
  => VoteSigningKey
  -> tosign
  -> Shelley.SignedDSIGN StandardCrypto tosign
  -> Bool
verify vsk payload sig =
  withVoteShelleySigningKey vsk $ \skey ->
    let
      v = getShelleyKeyWitnessVerificationKey skey
    in
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
