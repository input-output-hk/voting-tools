{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Catalyst.Crypto
  ( -- * SigningKey (private key)
   StakeSigningKey
  -- ** Creation
  , readStakeSigningKeyFile
  -- *** From Cardano.Api types
  , signingKeyFromStakeSigningKey
  , signingKeyFromStakeExtendedSigningKey
  -- ** Conversion
  -- *** To Cardano.Api types
  , withStakeSigningKey
  , withShelleySigningKey
  -- *** To verification key
  , getStakeVerificationKey
  -- * VerificationKey (public key)
  , StakeVerificationKey
  , AsType(AsStakeVerificationKey)
  , stakeVerificationKeyHash
  , serialiseStakeVerificationKeyToBech32
  -- ** Conversion
  , withStakeVerificationKey
  -- ** StakeAddress
  , stakeAddressFromKeyHash
  , stakeAddressFromVerificationKey
  -- * Operations
  , hashPayload
  , sign
  , verify
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
import           Data.Text (Text)

import           Cardano.Ledger.Crypto (Crypto (..), StandardCrypto)

data StakeSigningKey
  = AStakeSigningKey         (SigningKey StakeKey)
  | AStakeExtendedSigningKey (SigningKey StakeExtendedKey)
  deriving Show

instance Eq StakeSigningKey where
  skey1 == skey2 = getStakeVerificationKey skey1 == getStakeVerificationKey skey2

instance Ord StakeSigningKey where
  skey1 <= skey2 = getStakeVerificationKey skey1 <= getStakeVerificationKey skey2

data StakeVerificationKey
  = AStakeVerificationKey (VerificationKey StakeKey)
  | AStakeExtendedVerificationKey (VerificationKey StakeExtendedKey)
  deriving (Eq, Show)

instance Ord StakeVerificationKey where
  compare a b =
    compare (serialiseStakeVerificationKeyToBech32 a)
            (serialiseStakeVerificationKeyToBech32 b)

serialiseStakeVerificationKeyToBech32 :: StakeVerificationKey -> Text
serialiseStakeVerificationKeyToBech32 (AStakeVerificationKey verKey)
  = Api.serialiseToBech32 verKey
serialiseStakeVerificationKeyToBech32 (AStakeExtendedVerificationKey verKey)
  = Api.serialiseToBech32 verKey

stakeVerificationKeyHash :: StakeVerificationKey -> Hash StakeKey
stakeVerificationKeyHash v = withStakeVerificationKey v (verificationKeyHash)

instance ToJSON StakeVerificationKey where
  toJSON = Aeson.String . ("0x" <>) . T.decodeUtf8 . serialiseToRawBytesHex

instance FromJSON StakeVerificationKey where
  parseJSON = Aeson.withText "StakeVerificationKey" $ \str -> case T.stripPrefix "0x" str of
    Nothing  -> fail "Missing hex identifier '0x'."
    Just hex ->
      case deserialiseFromRawBytesHex AsStakeVerificationKey $ T.encodeUtf8 hex of
        Nothing -> fail "Failed to deserialise vote verification key."
        Just votePub -> pure votePub

getStakeVerificationKey :: StakeSigningKey -> StakeVerificationKey
getStakeVerificationKey (AStakeSigningKey skey)         = AStakeVerificationKey         $ getVerificationKey skey
getStakeVerificationKey (AStakeExtendedSigningKey skey) = AStakeExtendedVerificationKey $ getVerificationKey skey

withStakeVerificationKey :: StakeVerificationKey -> (VerificationKey StakeKey -> a) -> a
withStakeVerificationKey ver f =
  let
    vkey = case ver of
      (AStakeVerificationKey vkey')         -> vkey'
      (AStakeExtendedVerificationKey vkey') -> castVerificationKey vkey'
  in
    f vkey

stakeAddressFromKeyHash :: NetworkId -> Hash StakeKey -> StakeAddress
stakeAddressFromKeyHash nw = makeStakeAddress nw . StakeCredentialByKey

stakeAddressFromVerificationKey :: NetworkId -> StakeVerificationKey -> StakeAddress
stakeAddressFromVerificationKey nw = stakeAddressFromKeyHash nw . stakeVerificationKeyHash

hashPayload :: ByteString -> ByteString
hashPayload payload = Crypto.hashToBytes . Crypto.hashRaw $ LBS.fromStrict payload

sign :: ByteString -> StakeSigningKey -> Crypto.SigDSIGN (DSIGN StandardCrypto)
sign payload vsk = sign' (hashPayload payload) vsk

sign'
  :: Crypto.SignableRepresentation tosign
  => tosign
  -> StakeSigningKey
  -> Crypto.SigDSIGN (DSIGN StandardCrypto)
sign' payload vsk =
  withShelleySigningKey vsk $ \skey ->
    let
      (Crypto.SignedDSIGN sig) = makeShelleySignature payload skey
    in sig

verify
  :: StakeVerificationKey
  -> ByteString
  -> Crypto.SigDSIGN (DSIGN StandardCrypto)
  -> Bool
verify vkey payload sig = verify' vkey (hashPayload payload) sig

verify'
  :: Crypto.SignableRepresentation tosign
  => StakeVerificationKey
  -> tosign
  -> Crypto.SigDSIGN (DSIGN StandardCrypto)
  -> Bool
verify' vkey payload sig =
  withStakeVerificationKey vkey $ \(StakeVerificationKey (Shelley.VKey v)) ->
    either (const False) (const True) $ Crypto.verifyDSIGN () v payload sig

withStakeSigningKey :: StakeSigningKey
                   -> (forall keyrole. Key keyrole => SigningKey keyrole -> a)
                   -> a
withStakeSigningKey vsk f =
  case vsk of
    AStakeSigningKey sk         -> f sk
    AStakeExtendedSigningKey sk -> f sk

withShelleySigningKey :: StakeSigningKey -> (ShelleySigningKey -> a) -> a
withShelleySigningKey vsk f =
  case vsk of
    AStakeSigningKey (StakeSigningKey dsign)                -> f ( toShelleySigningKey $ WitnessStakeKey (StakeSigningKey dsign))
    AStakeExtendedSigningKey (StakeExtendedSigningKey xprv) -> f ( toShelleySigningKey $ WitnessStakeExtendedKey (StakeExtendedSigningKey xprv))

signingKeyFromStakeSigningKey :: SigningKey StakeKey -> StakeSigningKey
signingKeyFromStakeSigningKey sk = AStakeSigningKey sk

signingKeyFromStakeExtendedSigningKey :: SigningKey StakeExtendedKey -> StakeSigningKey
signingKeyFromStakeExtendedSigningKey sk = AStakeExtendedSigningKey sk

readStakeSigningKeyFile
  :: ( MonadIO m
     , MonadError e m
     , AsFileError e fileErr
     , AsInputDecodeError fileErr
     )
  => SigningKeyFile
  -> m StakeSigningKey
readStakeSigningKeyFile skFile =
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

instance HasTypeProxy StakeVerificationKey where
  data AsType StakeVerificationKey = AsStakeVerificationKey
  proxyToAsType _ = AsStakeVerificationKey

instance SerialiseAsRawBytes StakeVerificationKey where
  serialiseToRawBytes (AStakeVerificationKey vkey)         = serialiseToRawBytes vkey
  serialiseToRawBytes (AStakeExtendedVerificationKey vkey) = serialiseToRawBytes vkey

  deserialiseFromRawBytes AsStakeVerificationKey bs =
    case (AStakeExtendedVerificationKey <$> deserialiseFromRawBytes (AsVerificationKey AsStakeExtendedKey) bs) of
      Nothing -> (AStakeVerificationKey <$> deserialiseFromRawBytes (AsVerificationKey AsStakeKey) bs)
      x       -> x
