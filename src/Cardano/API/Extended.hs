-- | Cardano.API.Extended.Raw but I've made the errors "classy". Plus
-- some utility functions.

{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.API.Extended ( queryUTxOFromLocalState
                            , queryPParamsFromLocalState
                            , Extended.ShelleyQueryCmdLocalStateQueryError(..)
                            , AsShelleyQueryCmdLocalStateQueryError(..)
                            , readSigningKeyFile
                            , AsFileError(..)
                            , AsInputDecodeError(..)
                            , AsEnvSocketError(..)
                            , Extended.readerFromAttoParser
                            , Extended.parseAddress
                            , Extended.pNetworkId
                            , readEnvSocketPath
                            , Extended.textEnvelopeToJSON
                            , AsBech32DecodeError(..)
                            , AsBech32HumanReadablePartError(..)
                            , Bech32HumanReadablePartError(Bech32HumanReadablePartError)
                            , VotingKeyPublic
                            , deserialiseFromBech32
                            , AsType(AsVotingKeyPublic)
                            ) where

import Control.Monad (guard)
import Control.Monad.Trans.Except.Extra (firstExceptT, left, right, newExceptT)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, ExceptT, throwError, runExceptT)
import Control.Lens ((#))
import Control.Lens.TH (makeClassyPrisms)
import           Data.ByteString (ByteString)
import           Data.Text (Text)

import Cardano.API (HasTextEnvelope, SerialiseAsBech32, SigningKey, AsType, Bech32DecodeError)
import Cardano.Api.Typed (FileError(FileError, FileIOError))
import           Cardano.Api.Typed (HasTypeProxy(proxyToAsType), AsType, SerialiseAsRawBytes(serialiseToRawBytes, deserialiseFromRawBytes), Bech32DecodeError(Bech32DecodingError, Bech32UnexpectedPrefix, Bech32DataPartToBytesError, Bech32DeserialiseFromBytesError, Bech32WrongPrefix))
import Cardano.Api.Typed (StandardShelley, LocalNodeConnectInfo(LocalNodeConnectInfo), localNodeConsensusMode, NodeConsensusMode(ByronMode, ShelleyMode, CardanoMode), Address, Shelley, FileError)
import Cardano.CLI.Environment ( EnvSocketError(..))
import qualified Cardano.CLI.Environment as Cardano (readEnvSocketPath) 
import Cardano.CLI.Shelley.Key (InputDecodeError)
import Cardano.CLI.Types (SigningKeyFile, QueryFilter, SocketPath)
import Ouroboros.Network.Block (Tip)
import qualified Codec.Binary.Bech32 as Bech32
import Shelley.Spec.Ledger.PParams (PParams)
import qualified Cardano.CLI.Shelley.Key as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Ledger

import qualified Cardano.API.Extended.Raw as Extended

makeClassyPrisms ''Extended.ShelleyQueryCmdLocalStateQueryError
makeClassyPrisms ''FileError
makeClassyPrisms ''InputDecodeError
makeClassyPrisms ''EnvSocketError

makeClassyPrisms ''Bech32DecodeError

data Bech32HumanReadablePartError = Bech32HumanReadablePartError !(Bech32.HumanReadablePartError)
  deriving Show

makeClassyPrisms ''Bech32HumanReadablePartError

liftExceptTIO
  :: ( MonadIO m
     , MonadError e' m
     )
  => (e -> e') -> ExceptT e IO a -> m a
liftExceptTIO f exc = do
  x <- liftIO $ runExceptT exc
  either (throwError . f) pure x

queryUTxOFromLocalState
  :: ( MonadIO m
     , MonadError e m
     , AsShelleyQueryCmdLocalStateQueryError e
     )
  => QueryFilter
  -> LocalNodeConnectInfo mode block
  -> m (Ledger.UTxO StandardShelley)
queryUTxOFromLocalState qf =
  liftExceptTIO (_ShelleyQueryCmdLocalStateQueryError #) .
    Extended.queryUTxOFromLocalState qf

queryPParamsFromLocalState
  :: ( MonadIO m
     , MonadError e m
     , AsShelleyQueryCmdLocalStateQueryError e
     )
  => LocalNodeConnectInfo mode block
  -> m (PParams StandardShelley)
queryPParamsFromLocalState = 
  liftExceptTIO (_ShelleyQueryCmdLocalStateQueryError #) .
    Extended.queryPParamsFromLocalState

readSigningKeyFile
  :: forall e m fileErr keyrole .
     ( MonadIO m
     , MonadError e m
     , AsFileError e fileErr
     , AsInputDecodeError fileErr
     , HasTextEnvelope (SigningKey keyrole)
     , SerialiseAsBech32 (SigningKey keyrole)
     )
  => AsType keyrole
  -> SigningKeyFile
  -> m (SigningKey keyrole)
readSigningKeyFile role f = do
  result <- liftIO $ Shelley.readSigningKeyFile role f
  case result of
    Right x                 -> pure x
    Left (FileError fp e)   -> throwError (_FileError # (fp , _InputDecodeError # e))
    Left (FileIOError fp e) -> throwError (_FileIOError # (fp, e))

readEnvSocketPath
  :: ( MonadIO m
     , MonadError e m
     , AsEnvSocketError e
     )
  => m SocketPath
readEnvSocketPath =
  liftExceptTIO (_EnvSocketError #)
    Cardano.readEnvSocketPath

-- | Voting key types do not exist in the cardano-api yet. This
-- extension adds voting keys, but makes no guarantees that the
-- contents of the voting key are correct. It does however provide the
-- standard interfaces for serialising and deserialising voting keys.

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
-- for me to be able to define serialization for new types.

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

-- | End voting keys
