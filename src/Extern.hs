{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Extern where

import Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, left, newExceptT, handleIOExceptT)
import Control.Monad.Except (ExceptT, throwError, MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Lens ((#))
import Control.Lens.TH (makeClassyPrisms)
import Control.Exception.Safe (IOException, try)

import qualified Codec.Binary.Bech32 as Bech32
import Cardano.CLI.Shelley.Key (InputDecodeError, readSigningKeyFileAnyOf)
import Cardano.CLI.Shelley.Run.Key (SomeSigningKey(..))
import Cardano.CLI.Shelley.Run.Query (ShelleyQueryCmdError)
import Cardano.CLI.Types (SigningKeyFile (..), VerificationKeyFile (..), SocketPath(SocketPath), QueryFilter(NoFilter, FilterByAddress))
import Cardano.Api.Typed (FileError, LocalNodeConnectInfo(..), FromSomeType(FromSomeType), AsType(..), Key, SigningKey, NetworkId, VerificationKey, StandardShelley, NodeConsensusMode(ByronMode, ShelleyMode, CardanoMode), Address(ShelleyAddress, ByronAddress), Shelley, TxMetadataValue(TxMetaMap, TxMetaNumber, TxMetaText))
import Cardano.Api.Protocol (withlocalNodeConnectInfo, Protocol(..))
import Cardano.API (queryNodeLocalState, getVerificationKey, StakeKey, serialiseToRawBytes, serialiseToRawBytesHex, deserialiseFromBech32, TxMetadata, Bech32DecodeError, makeTransactionMetadata)
import Cardano.CLI.Environment ( EnvSocketError(..) , readEnvSocketPath)
import Cardano.Api.LocalChainSync ( getLocalTip )
import Ouroboros.Network.Block (Tip, getTipPoint)
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..), Either(QueryResultSuccess, QueryResultEraMismatch), Query(QueryIfCurrentShelley), EraMismatch (..))
import Ouroboros.Consensus.Shelley.Ledger.Query (Query(..))
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate (Either (DegenQueryResult),
                     Query (DegenQuery))
import qualified Shelley.Spec.Ledger.Address as Ledger
import qualified Shelley.Spec.Ledger.UTxO as Ledger
import           Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery (AcquireFailure (..))

import CLI.Jormungandr
import Encoding

data Bech32HumanReadablePartError = Bech32HumanReadablePartError !(Bech32.HumanReadablePartError)

makeClassyPrisms ''FileError
makeClassyPrisms ''Bech32DecodeError
makeClassyPrisms ''Bech32HumanReadablePartError

readSigningKeyFile
  :: SigningKeyFile
  -> ExceptT (FileError InputDecodeError) IO SomeSigningKey
readSigningKeyFile skFile =
    newExceptT $
      readSigningKeyFileAnyOf bech32FileTypes textEnvFileTypes skFile
  where
    textEnvFileTypes =
      [ FromSomeType (AsSigningKey AsByronKey)
                      AByronSigningKey
      , FromSomeType (AsSigningKey AsPaymentKey)
                      APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                      APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)
                      AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                      AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)
                      AStakePoolSigningKey
      , FromSomeType (AsSigningKey AsGenesisKey)
                      AGenesisSigningKey
      , FromSomeType (AsSigningKey AsGenesisExtendedKey)
                      AGenesisExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateKey)
                      AGenesisDelegateSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey)
                      AGenesisDelegateExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisUTxOKey)
                      AGenesisUTxOSigningKey
      , FromSomeType (AsSigningKey AsVrfKey)
                      AVrfSigningKey
      , FromSomeType (AsSigningKey AsKesKey)
                      AKesSigningKey
      ]

    bech32FileTypes =
      [ FromSomeType (AsSigningKey AsByronKey)
                      AByronSigningKey
      , FromSomeType (AsSigningKey AsPaymentKey)
                      APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                      APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)
                      AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                      AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)
                      AStakePoolSigningKey
      , FromSomeType (AsSigningKey AsVrfKey)
                      AVrfSigningKey
      , FromSomeType (AsSigningKey AsKesKey)
                      AKesSigningKey
      ]

withSomeSigningKey :: SomeSigningKey
                   -> (forall keyrole. Key keyrole => SigningKey keyrole -> a)
                   -> a
withSomeSigningKey ssk f =
    case ssk of
      AByronSigningKey           sk -> f sk
      APaymentSigningKey         sk -> f sk
      APaymentExtendedSigningKey sk -> f sk
      AStakeSigningKey           sk -> f sk
      AStakeExtendedSigningKey   sk -> f sk
      AStakePoolSigningKey       sk -> f sk
      AGenesisSigningKey         sk -> f sk
      AGenesisExtendedSigningKey sk -> f sk
      AGenesisDelegateSigningKey sk -> f sk
      AGenesisDelegateExtendedSigningKey
                                 sk -> f sk
      AGenesisUTxOSigningKey     sk -> f sk
      AVrfSigningKey             sk -> f sk
      AKesSigningKey             sk -> f sk

data VoterRegistrationError
  = VoterEnvSocketError !EnvSocketError
  -- ^ Unable to retrieve the socket path
  | VoterQueryAcquireFailureError !LocalStateQuery.AcquireFailure
  -- ^ Cannot obtain the state for the requested point.
  | VoterByronProtocolNotSupportedError
  -- ^ Tried to use a node running in Byron mode
  | VoterQueryEraMismatchError !EraMismatch
  -- ^ Caused by applying a query from one era to a ledger from a
  -- different era.
  | VoterFileError !(FileError InputDecodeError)
  -- ^ Errors reading from a file
  | VoterNotStakeSigningKey !SomeSigningKey
  -- ^ User provided stake signing key is not a "StakeSigningKey" but some other signing key

all :: Protocol -> NetworkId -> Set (Address Shelley) -> SigningKeyFile -> FilePath -> ExceptT VoterRegistrationError IO (Ledger.UTxO StandardShelley)
all protocol network as skf votekf = do
  SocketPath sockPath <- firstExceptT VoterEnvSocketError readEnvSocketPath
  withlocalNodeConnectInfo protocol network sockPath $ \connectInfo -> do
    tip     <- liftIO $ getLocalTip connectInfo
    utxos   <- queryUTxOFromLocalState connectInfo tip (FilterByAddress as)
    stkSign <- readStakeSigningKey skf
    let stkVerify = getVerificationKey stkSign
    
    -- meta      <- generateVoteMetadata stkSign stkVerify votekf

    pure utxos

generateVoteMetadata
  :: ( MonadIO m
     , MonadError e m
     , AsFileError e d
     -- TODO UTF8 DecodeError
     , AsDecodeError e
     , AsBech32DecodeError e
     , AsBech32HumanReadablePartError e
     )
  => SigningKey StakeKey
  -> VerificationKey StakeKey
  -> FilePath
  -> m TxMetadata
generateVoteMetadata stkSign stkVerify votekf = do
  votepk        <- readVotePublicKey votekf
  votepkBytes   <- jcliKeyToBytes votepk
  jcliStkSign   <- jcliKeyFromBytes (serialiseToRawBytes stkSign) 
  jcliStkPublic <- jcliKeyPublic jcliStkSign
  sig           <- jcliSign jcliStkSign votepkBytes
  hexSig        <- decodeBytesUtf8 =<< bech32SignatureToHex sig
  stkVerifyHex  <- decodeBytesUtf8 $ serialiseToRawBytesHex stkVerify

  -- Verify
  x <- newPrefix "ed25519_pk" stkVerifyHex
  y <- newPrefix "ed25519_sig" hexSig
  jcliValidateSig x y votepkBytes
  
  pure $ makeTransactionMetadata $ M.fromList [(1, TxMetaMap
                          [ ( TxMetaText "purpose"    , TxMetaText "voting_registration" )
                          , ( TxMetaText "voting_key" , TxMetaText $ "0x" <> votepkBytes   )
                          , ( TxMetaText "stake_pub"  , TxMetaText $ "0x" <> stkVerifyHex  )
                          , ( TxMetaText "signature"  , TxMetaText $ "0x" <> hexSig        )
                          ]
                        )
                       ]

-- validateSig 

newPrefix hrPartTxt x = do
  case Bech32.humanReadablePartFromText hrPartTxt of
    Left bech32HrPartErr -> throwError $ (_Bech32HumanReadablePartError #) bech32HrPartErr
    Right hrPart ->
      case Bech32.decodeLenient x of
        Left bech32DecodeErr -> throwError $ (_Bech32DecodingError #) bech32DecodeErr
        Right (_, dataPart) -> pure $ Bech32.encodeLenient hrPart dataPart


bech32SignatureToHex
  :: ( MonadError e m
     , AsBech32DecodeError e
     )
  => Text
  -> m BS.ByteString
bech32SignatureToHex sig =
  -- TODO, is this the type I want to deserialize to?
  case deserialiseFromBech32 (AsSigningKey AsStakeKey) sig of
    Left bech32Err -> throwError $ (_Bech32DecodeError #) bech32Err 
    Right x -> pure $ serialiseToRawBytesHex x

readVotePublicKey
  :: ( MonadIO m
     , MonadError e m
     , AsFileError e d
     )
  => FilePath
  -> m Text
readVotePublicKey path = do
  result <- liftIO . try $ TIO.readFile path
  either (\e -> throwError . (_FileIOError #) $ (path, e)) pure result

readStakeSigningKey :: SigningKeyFile -> ExceptT VoterRegistrationError IO (SigningKey StakeKey)
readStakeSigningKey skf = do
  ssk       <- firstExceptT VoterFileError $ readSigningKeyFile skf
  case ssk of
    AStakeSigningKey sk -> pure sk
    sk                  -> throwError $ VoterNotStakeSigningKey sk

queryUTxOFromLocalState
  :: LocalNodeConnectInfo mode block
  -> Tip block
  -> QueryFilter
  -> ExceptT VoterRegistrationError IO (Ledger.UTxO StandardShelley)
queryUTxOFromLocalState connectInfo@LocalNodeConnectInfo{localNodeConsensusMode} tip qFilter =
  case localNodeConsensusMode of
    ByronMode{} -> throwError VoterByronProtocolNotSupportedError

    ShelleyMode{} -> do
      DegenQueryResult result <- firstExceptT VoterQueryAcquireFailureError . newExceptT $
        queryNodeLocalState
          connectInfo
          (getTipPoint tip, DegenQuery (applyUTxOFilter qFilter))
      return result

    CardanoMode{} -> do
      result <- firstExceptT VoterQueryAcquireFailureError . newExceptT $
        queryNodeLocalState
          connectInfo
          (getTipPoint tip, QueryIfCurrentShelley (applyUTxOFilter qFilter))
      case result of
        QueryResultEraMismatch err -> throwError (VoterQueryEraMismatchError err)
        QueryResultSuccess utxo -> return utxo
  where
    applyUTxOFilter (FilterByAddress as) = GetFilteredUTxO (toShelleyAddrs as)
    applyUTxOFilter NoFilter             = GetUTxO

    toShelleyAddrs :: Set (Address Shelley) -> Set (Ledger.Addr StandardShelley)
    toShelleyAddrs = Set.map toShelleyAddr

    toShelleyAddr :: Address era -> Ledger.Addr StandardShelley
    toShelleyAddr (ByronAddress addr)        = Ledger.AddrBootstrap
                                                 (Ledger.BootstrapAddress addr)
    toShelleyAddr (ShelleyAddress nw pc scr) = Ledger.Addr nw pc scr


-- -- cardano.build_tx("meta.txbody", txins=txins, txouts=txouts, ttl=tip["slotNo"], metadata = meta)
-- -- fee = cardano.estimate_fee(len(txins), len(txouts), 1, txbody_file="meta.txbody")
-- -- txouts = [( arguments["--payment-address"], value - fee )]
-- -- cardano.build_tx("meta.txbody", txins=txins, txouts=txouts, ttl=tip["slotNo"] + 5000, metadata=meta, fee=fee)
-- -- cardano.sign_tx("meta.txbody", "meta.txsigned", [ arguments["--payment-signing-key"] ])
