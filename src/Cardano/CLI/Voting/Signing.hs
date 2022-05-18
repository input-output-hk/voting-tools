{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.Voting.Signing ( VoteSigningKey
                                  , VoteVerificationKey
                                  , AsType(AsVoteVerificationKey)
                                  , getVoteVerificationKey
                                  , withVoteVerificationKey
                                  , serialiseVoteVerificationKeyToBech32
                                  , voteVerificationKeyHashRaw
                                  , voteVerificationKeyStakeAddressHashRaw
                                  , hashVotePayload
                                  , withVoteSigningKey
                                  , withVoteShelleySigningKey
                                  , voteSigningKeyFromStakeSigningKey
                                  , voteSigningKeyFromStakeExtendedSigningKey
                                  , getStakeHash
                                  , sign
                                  , verify
                                  , readVoteSigningKeyFile
                                  , verificationKeyRawBytes
                                  , toStakeAddr
                                  , voteVerificationKeyToStakeAddress
                                  ) where

import           Control.Monad.Except (MonadError)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Cardano.API.Extended (AsFileError, AsInputDecodeError, readSigningKeyFileAnyOf)
import           Cardano.Api
                   (AsType (AsSigningKey, AsStakeExtendedKey, AsStakeKey, AsVerificationKey),
                   FromSomeType (..), HasTypeProxy, Hash, Key, NetworkId,
                   SerialiseAsRawBytes (deserialiseFromRawBytes, serialiseToRawBytes), SigningKey,
                   StakeAddress, StakeExtendedKey, StakeKey, VerificationKey, castVerificationKey,
                   deserialiseFromRawBytesHex, getVerificationKey, makeStakeAddress, proxyToAsType,
                   serialiseToRawBytes, serialiseToRawBytesHex, verificationKeyHash)
import           Cardano.Api.Shelley (ShelleySigningKey, ShelleyWitnessSigningKey (..),
                   SigningKey (..), StakeCredential (..), VerificationKey (StakeVerificationKey),
                   makeShelleySignature, toShelleySigningKey)
import           Cardano.CLI.Types (SigningKeyFile)
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Util as Crypto
import qualified Cardano.Ledger.Keys as Shelley

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api
import qualified Data.ByteString.Char8 as BC
import           Data.Text (Text)

import           Cardano.Ledger.Crypto (Crypto (..), StandardCrypto)

data VoteSigningKey
  = AStakeSigningKey         (SigningKey StakeKey)
  | AStakeExtendedSigningKey (SigningKey StakeExtendedKey)
  deriving Show

instance Eq VoteSigningKey where
  skey1 == skey2 = getVoteVerificationKey skey1 == getVoteVerificationKey skey2

instance Ord VoteSigningKey where
  skey1 <= skey2 = getVoteVerificationKey skey1 <= getVoteVerificationKey skey2

data VoteVerificationKey
  = AStakeVerificationKey (VerificationKey StakeKey)
  | AStakeExtendedVerificationKey (VerificationKey StakeExtendedKey)
  deriving (Eq, Show)

instance Ord VoteVerificationKey where
  compare a b =
    compare (serialiseVoteVerificationKeyToBech32 a)
            (serialiseVoteVerificationKeyToBech32 b)

serialiseVoteVerificationKeyToBech32 :: VoteVerificationKey -> Text
serialiseVoteVerificationKeyToBech32 (AStakeVerificationKey verKey)
  = Api.serialiseToBech32 verKey
serialiseVoteVerificationKeyToBech32 (AStakeExtendedVerificationKey verKey)
  = Api.serialiseToBech32 verKey

voteVerificationKeyHashRaw :: VoteVerificationKey -> ByteString
voteVerificationKeyHashRaw (AStakeVerificationKey vKey)
  = Api.serialiseToRawBytesHex $ Api.verificationKeyHash vKey
voteVerificationKeyHashRaw (AStakeExtendedVerificationKey vKey)
  = Api.serialiseToRawBytesHex $ Api.verificationKeyHash vKey

voteVerificationKeyStakeAddressHashRaw :: NetworkId -> VoteVerificationKey -> ByteString
voteVerificationKeyStakeAddressHashRaw nw vKey =
  Api.serialiseToRawBytes
  $ Api.makeStakeAddress nw (Api.StakeCredentialByKey (getStakeHash vKey))

instance ToJSON VoteVerificationKey where
  toJSON = Aeson.String . ("0x" <>) . T.decodeUtf8 . serialiseToRawBytesHex

instance FromJSON VoteVerificationKey where
  parseJSON = Aeson.withText "VoteVerificationKey" $ \str -> case T.stripPrefix "0x" str of
    Nothing  -> fail "Missing hex identifier '0x'."
    Just hex ->
      case deserialiseFromRawBytesHex AsVoteVerificationKey $ T.encodeUtf8 hex of
        Nothing -> fail "Failed to deserialise vote verification key."
        Just votePub -> pure votePub

getVoteVerificationKey :: VoteSigningKey -> VoteVerificationKey
getVoteVerificationKey (AStakeSigningKey skey)         = AStakeVerificationKey         $ getVerificationKey skey
getVoteVerificationKey (AStakeExtendedSigningKey skey) = AStakeExtendedVerificationKey $ getVerificationKey skey

getStakeHash :: VoteVerificationKey -> Hash StakeKey
getStakeHash v = withVoteVerificationKey v (verificationKeyHash)

withVoteVerificationKey :: VoteVerificationKey -> (VerificationKey StakeKey -> a) -> a
withVoteVerificationKey ver f =
  let
    vkey = case ver of
      (AStakeVerificationKey vkey')         -> vkey'
      (AStakeExtendedVerificationKey vkey') -> castVerificationKey vkey'
  in
    f vkey

toStakeAddr :: NetworkId -> Hash StakeKey -> StakeAddress
toStakeAddr nw = makeStakeAddress nw . StakeCredentialByKey

verificationKeyRawBytes :: VoteSigningKey -> ByteString
verificationKeyRawBytes (AStakeSigningKey k)         = serialiseToRawBytes $ getVerificationKey k
verificationKeyRawBytes (AStakeExtendedSigningKey k) = serialiseToRawBytes $ getVerificationKey k

hashVotePayload :: ByteString -> ByteString
hashVotePayload payload = Crypto.hashToBytes . Crypto.hashRaw $ LBS.fromStrict payload

sign :: ByteString -> VoteSigningKey -> Crypto.SigDSIGN (DSIGN StandardCrypto)
sign payload vsk = sign' (hashVotePayload payload) vsk

sign'
  :: Crypto.SignableRepresentation tosign
  => tosign
  -> VoteSigningKey
  -> Crypto.SigDSIGN (DSIGN StandardCrypto)
sign' payload vsk =
  withVoteShelleySigningKey vsk $ \skey ->
    let
      (Crypto.SignedDSIGN sig) = makeShelleySignature payload skey
    in sig

verify
  :: VoteVerificationKey
  -> ByteString
  -> Crypto.SigDSIGN (DSIGN StandardCrypto)
  -> Bool
verify vkey payload sig = verify' vkey (hashVotePayload payload) sig

verify'
  :: Crypto.SignableRepresentation tosign
  => VoteVerificationKey
  -> tosign
  -> Crypto.SigDSIGN (DSIGN StandardCrypto)
  -> Bool
verify' vkey payload sig =
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

voteSigningKeyFromStakeSigningKey :: SigningKey StakeKey -> VoteSigningKey
voteSigningKeyFromStakeSigningKey sk = AStakeSigningKey sk

voteSigningKeyFromStakeExtendedSigningKey :: SigningKey StakeExtendedKey -> VoteSigningKey
voteSigningKeyFromStakeExtendedSigningKey sk = AStakeExtendedSigningKey sk

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

voteVerificationKeyToStakeAddress :: NetworkId -> Text -> Either String Text
voteVerificationKeyToStakeAddress nw verKeyHex =
  case Aeson.fromJSON (Aeson.String verKeyHex) of
      Aeson.Error err -> Left err
      Aeson.Success verKey ->
          let
              stakeAddress = Api.makeStakeAddress nw (Api.StakeCredentialByKey (getStakeHash verKey))
              stakeAddressHex = T.pack $ BC.unpack $ Api.serialiseToRawBytesHex stakeAddress
          in
              Right stakeAddressHex
