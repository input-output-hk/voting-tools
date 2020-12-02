{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Voting key types do not exist in the cardano-api yet. This
-- extension adds voting keys, but makes no guarantees that the
-- contents of the voting key are correct. It does whoever provide the
-- standard interfaces for serialising and deserialising voting keys.

module Cardano.API.Voting ( VotingKeyPublic
                          , deserialiseFromBech32
                          , AsType(AsVotingKeyPublic)
                          , readVotePublicKey
                          ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Except (MonadError, throwError)
import           Control.Exception.Safe (try)
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Codec.Binary.Bech32 as Bech32
import Control.Monad (guard)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Control.Lens ((#))

import           Cardano.Api.Typed (HasTypeProxy(proxyToAsType), AsType, SerialiseAsRawBytes(serialiseToRawBytes, deserialiseFromRawBytes), Bech32DecodeError(Bech32DecodingError, Bech32UnexpectedPrefix, Bech32DataPartToBytesError, Bech32DeserialiseFromBytesError, Bech32WrongPrefix))

import           Cardano.API.Extended (AsFileError(_FileIOError))
import           Encoding (AsBech32DecodeError(_Bech32DecodeError))
import           CLI.Interop (stripTrailingNewlines)


data VotingKeyPublic = VotingKeyPublic { votingKeyPublicRawBytes :: ByteString }
  deriving (Eq, Show)

instance HasTypeProxy VotingKeyPublic where
  data AsType VotingKeyPublic = AsVotingKeyPublic
  proxyToAsType _ = AsVotingKeyPublic

instance SerialiseAsRawBytes VotingKeyPublic where
  serialiseToRawBytes (VotingKeyPublic raw) = raw
  deserialiseFromRawBytes AsVotingKeyPublic = Just . VotingKeyPublic

-- TODO Ask for this class to be exposed in Cardano.API...
-- The SerialiseAsBech32 class need to be exposed from the CardanoAPI
-- for me to be able to defined serialization for new types.

-- instance SerialiseAsBech32 VotingKeyPublic where
--   bech32PrefixFor (VotingKeyPublic) = "ed25519e_sk"

--   bech32PrefixesPermitted AsVotingKeyPublic = ["ed25519e_sk"]

(?!.) :: Either e a -> (e -> e') -> Either e' a
Left  e ?!. f = Left (f e)
Right x ?!. _ = Right x

(?!) :: Maybe a -> e -> Either e a
Nothing ?! e = Left e
Just x  ?! _ = Right x

votingPublicKeyBech32Prefix = "ed25519e_sk"
  
deserialiseFromBech32 :: AsType VotingKeyPublic -> Text -> Either Bech32DecodeError VotingKeyPublic
deserialiseFromBech32 asType bech32Str = do
    (prefix, dataPart) <- Bech32.decodeLenient bech32Str
                            ?!. Bech32DecodingError

    let actualPrefix      = Bech32.humanReadablePartToText prefix
        permittedPrefixes = [votingPublicKeyBech32Prefix]
    guard (actualPrefix `elem` permittedPrefixes)
      ?! Bech32UnexpectedPrefix actualPrefix (Set.fromList permittedPrefixes)

    payload <- Bech32.dataPartToBytes dataPart
                 ?! Bech32DataPartToBytesError (Bech32.dataPartToText dataPart)

    value <- deserialiseFromRawBytes asType payload
               ?! Bech32DeserialiseFromBytesError payload

    let expectedPrefix = votingPublicKeyBech32Prefix
    guard (actualPrefix == expectedPrefix)
      ?! Bech32WrongPrefix actualPrefix expectedPrefix

    return value

serialiseToBech32 :: VotingKeyPublic -> Text
serialiseToBech32 a =
    Bech32.encodeLenient
      humanReadablePart
      (Bech32.dataPartFromBytes (serialiseToRawBytes a))
  where
    humanReadablePart =
      case Bech32.humanReadablePartFromText (votingPublicKeyBech32Prefix) of
        Right p  -> p
        Left err -> error $ "serialiseToBech32: invalid prefix "
                         ++ show votingPublicKeyBech32Prefix
                         ++ ", " ++ show err

readVotePublicKey
  :: ( MonadIO m
     , MonadError e m
     , AsFileError e d
     , AsBech32DecodeError e
     )
  => FilePath
  -> m VotingKeyPublic
readVotePublicKey path = do
  result <- liftIO . try $ TIO.readFile path
  raw    <- either (\e -> throwError . (_FileIOError #) $ (path, e)) pure result
  let publicKeyBech32 = stripTrailingNewlines raw
  either (throwError . (_Bech32DecodeError #)) pure $ deserialiseFromBech32 AsVotingKeyPublic publicKeyBech32
