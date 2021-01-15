-- | Cardano.API.Extended.Raw but I've made the errors "classy". Plus
-- some utility functions.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.API.Extended ( queryUTxOFromLocalState
                            , queryPParamsFromLocalState
                            , Extended.ShelleyQueryCmdLocalStateQueryError(..)
                            , AsShelleyQueryCmdLocalStateQueryError(..)
                            , readSigningKeyFile
                            , readSigningKeyFileAnyOf
                            , AsFileError(..)
                            , AsInputDecodeError(..)
                            , AsEnvSocketError(..)
                            , Extended.readerFromAttoParser
                            , Extended.parseAddressAny
                            , Extended.pNetworkId
                            , readEnvSocketPath
                            , Extended.textEnvelopeToJSON
                            , AsBech32DecodeError(..)
                            , AsBech32HumanReadablePartError(..)
                            , Bech32HumanReadablePartError(Bech32HumanReadablePartError)
                            , VotingKeyPublic
                            , deserialiseFromBech32'
                            , serialiseToBech32'
                            , liftShelleyBasedEra
                            , liftShelleyBasedMetadata
                            , liftShelleyBasedTxFee
                            , SerialiseAsBech32'(bech32PrefixFor, bech32PrefixesPermitted)
                            , AsType(AsVotingKeyPublic)
                            ) where

import           Control.Lens (( # ))
import           Control.Lens.TH (makeClassyPrisms)
import           Control.Monad (guard)
import           Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT, right)
import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BC
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T

import           Cardano.API (AnyCardanoEra (AnyCardanoEra), AsType, Bech32DecodeError,
                     CardanoEra (AllegraEra, MaryEra, ShelleyEra),
                     CardanoEraStyle (ShelleyBasedEra), FromSomeType, HasTextEnvelope,
                     IsShelleyBasedEra, Lovelace, SerialiseAsBech32,
                     ShelleyBasedEra (ShelleyBasedEraAllegra, ShelleyBasedEraMary, ShelleyBasedEraShelley),
                     SigningKey, TxFee (TxFeeExplicit),
                     TxFeesExplicitInEra (TxFeesExplicitInAllegraEra, TxFeesExplicitInMaryEra, TxFeesExplicitInShelleyEra),
                     TxMetadata, TxMetadataInEra (TxMetadataInEra),
                     TxMetadataSupportedInEra (TxMetadataInAllegraEra, TxMetadataInMaryEra, TxMetadataInShelleyEra),
                     cardanoEraStyle)
import           Cardano.Api.Shelley (ShelleyBasedEra)
import           Cardano.Api.Typed (FileError (FileError, FileIOError), ShelleyLedgerEra)
import           Cardano.Api.Typed (AsType, Bech32DecodeError (Bech32DataPartToBytesError, Bech32DecodingError, Bech32DeserialiseFromBytesError, Bech32UnexpectedPrefix, Bech32WrongPrefix),
                     HasTypeProxy (proxyToAsType),
                     SerialiseAsRawBytes (deserialiseFromRawBytes, serialiseToRawBytes))
import           Cardano.Api.Typed (Address, FileError, LocalNodeConnectInfo (LocalNodeConnectInfo),
                     NodeConsensusMode (ByronMode, CardanoMode, ShelleyMode), Shelley,
                     StandardShelley, localNodeConsensusMode)
import           Cardano.CLI.Environment (EnvSocketError (..))
import qualified Cardano.CLI.Environment as Cardano (readEnvSocketPath)
import           Cardano.CLI.Shelley.Key (InputDecodeError)
import qualified Cardano.CLI.Shelley.Key as Shelley
import           Cardano.CLI.Types (QueryFilter, SigningKeyFile, SocketPath)
import qualified Codec.Binary.Bech32 as Bech32
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import           Ouroboros.Network.Block (Tip)
import           Shelley.Spec.Ledger.PParams (PParams)
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

liftShelleyBasedTxFee
  :: ShelleyBasedEra era
  -> Lovelace
  -> TxFee era
liftShelleyBasedTxFee (ShelleyBasedEraShelley) = (TxFeeExplicit TxFeesExplicitInShelleyEra)
liftShelleyBasedTxFee (ShelleyBasedEraAllegra) = (TxFeeExplicit TxFeesExplicitInAllegraEra)
liftShelleyBasedTxFee (ShelleyBasedEraMary)    = (TxFeeExplicit TxFeesExplicitInMaryEra)

liftShelleyBasedMetadata
  :: ShelleyBasedEra era
  -> TxMetadata
  -> TxMetadataInEra era
liftShelleyBasedMetadata (ShelleyBasedEraShelley) = (TxMetadataInEra TxMetadataInShelleyEra)
liftShelleyBasedMetadata (ShelleyBasedEraAllegra) = (TxMetadataInEra TxMetadataInAllegraEra)
liftShelleyBasedMetadata (ShelleyBasedEraMary)    = (TxMetadataInEra TxMetadataInMaryEra)

liftShelleyBasedEra
  :: ShelleyBasedEra era
  -> CardanoEra era
liftShelleyBasedEra (ShelleyBasedEraShelley) = ShelleyEra
liftShelleyBasedEra (ShelleyBasedEraAllegra) = AllegraEra
liftShelleyBasedEra (ShelleyBasedEraMary)    = MaryEra

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
     , Consensus.ShelleyBasedEra ledgerera
     , ShelleyLedgerEra era ~ ledgerera
     , IsShelleyBasedEra era
     )
  => ShelleyBasedEra era
  -> QueryFilter
  -> LocalNodeConnectInfo mode block
  -> m (Ledger.UTxO ledgerera)
queryUTxOFromLocalState era qf =
  liftExceptTIO (_ShelleyQueryCmdLocalStateQueryError #) .
    Extended.queryUTxOFromLocalState era qf

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

readSigningKeyFileAnyOf
  :: forall e m fileErr b.
     ( MonadIO m
     , MonadError e m
     , AsFileError e fileErr
     , AsInputDecodeError fileErr
     )
  => [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> SigningKeyFile
  -> m b
readSigningKeyFileAnyOf bech32Types textEnvTypes f = do
  result <- liftIO $ Shelley.readSigningKeyFileAnyOf bech32Types textEnvTypes f
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

data VotingKeyPublic = VotingKeyPublic
    { votingKeyPublicRawBytes :: ByteString
    }
    deriving (Eq, Ord, Show)

instance HasTypeProxy VotingKeyPublic where
  data AsType VotingKeyPublic = AsVotingKeyPublic
  proxyToAsType _ = AsVotingKeyPublic

instance SerialiseAsRawBytes VotingKeyPublic where
  serialiseToRawBytes (VotingKeyPublic raw) = raw
  deserialiseFromRawBytes AsVotingKeyPublic = Just . VotingKeyPublic

instance SerialiseAsBech32' VotingKeyPublic where
    bech32PrefixFor (VotingKeyPublic _) = "ed25519_pk"
    bech32PrefixesPermitted AsVotingKeyPublic = ["ed25519_pk"]

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

class (HasTypeProxy a, SerialiseAsRawBytes a) => SerialiseAsBech32' a where

    -- | The human readable prefix to use when encoding this value to Bech32.
    --
    bech32PrefixFor :: a -> Text

    -- | The set of human readable prefixes that can be used for this type.
    --
    bech32PrefixesPermitted :: AsType a -> [Text]

serialiseToBech32' :: SerialiseAsBech32' a => a -> Text
serialiseToBech32' a =
    Bech32.encodeLenient
      humanReadablePart
      (Bech32.dataPartFromBytes (serialiseToRawBytes a))
  where
    humanReadablePart =
      case Bech32.humanReadablePartFromText (bech32PrefixFor a) of
        Right p  -> p
        Left err -> error $ "serialiseToBech32: invalid prefix "
                         ++ show (bech32PrefixFor a)
                         ++ ", " ++ show err


deserialiseFromBech32' :: SerialiseAsBech32' a
                      => AsType a -> Text -> Either Bech32DecodeError a
deserialiseFromBech32' asType bech32Str = do
    (prefix, dataPart) <- Bech32.decodeLenient bech32Str
                            ?!. Bech32DecodingError

    let actualPrefix      = Bech32.humanReadablePartToText prefix
        permittedPrefixes = bech32PrefixesPermitted asType
    guard (actualPrefix `elem` permittedPrefixes)
      ?! Bech32UnexpectedPrefix actualPrefix (Set.fromList permittedPrefixes)

    payload <- Bech32.dataPartToBytes dataPart
                 ?! Bech32DataPartToBytesError (Bech32.dataPartToText dataPart)

    value <- deserialiseFromRawBytes asType payload
               ?! Bech32DeserialiseFromBytesError payload

    let expectedPrefix = bech32PrefixFor value
    guard (actualPrefix == expectedPrefix)
      ?! Bech32WrongPrefix actualPrefix expectedPrefix

    return value
