{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Extern where

import Data.String (fromString)
import Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, left, newExceptT, handleIOExceptT)
import Control.Monad.Except (ExceptT, throwError, MonadError, runExceptT)
import qualified Data.ByteArray.Encoding as BA
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Set (Set)
import Control.Applicative ((<|>))
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as T
import Control.Lens ((#))
import Control.Lens.TH (makeClassyPrisms)
import Control.Exception.Safe (IOException, try)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Char8 as BSC
import qualified Options.Applicative as Opt
import qualified Data.ByteString.Base16 as Base16

import Cardano.API.Misc
import Cardano.Api.TextView (TextViewError, TextViewType(TextViewType))
import qualified Codec.Binary.Bech32 as Bech32
import Cardano.CLI.Shelley.Key (InputDecodeError, readSigningKeyFileAnyOf)
import Cardano.CLI.Shelley.Run.Key (SomeSigningKey(..))
import Cardano.CLI.Shelley.Run.Query (ShelleyQueryCmdError)
import Cardano.CLI.Types (SigningKeyFile (..), VerificationKeyFile (..), SocketPath(SocketPath), QueryFilter(NoFilter, FilterByAddress))
import Cardano.Api.Typed (FileError(FileError, FileIOError), LocalNodeConnectInfo(..), FromSomeType(FromSomeType), AsType(..), Key, SigningKey, NetworkId, VerificationKey, StandardShelley, NodeConsensusMode(ByronMode, ShelleyMode, CardanoMode), Address(ShelleyAddress, ByronAddress), Shelley, TxMetadataValue(TxMetaMap, TxMetaNumber, TxMetaText, TxMetaBytes), txCertificates, txWithdrawals, txMetadata, txUpdateProposal, TxOut(TxOut), TxId(TxId), TxIx(TxIx), NetworkMagic(NetworkMagic), ShelleyWitnessSigningKey(WitnessPaymentKey))
import Cardano.Api.Protocol (withlocalNodeConnectInfo, Protocol(..))
import Cardano.API (queryNodeLocalState, getVerificationKey, StakeKey, serialiseToRawBytes, serialiseToRawBytesHex, deserialiseFromBech32, TxMetadata, Bech32DecodeError, makeTransactionMetadata, makeShelleyTransaction, txExtraContentEmpty, Lovelace(Lovelace), TxIn(TxIn), estimateTransactionFee, makeSignedTransaction, Witness, Tx, deserialiseAddress, TextEnvelopeError, readFileTextEnvelope, NetworkMagic, NetworkId(Testnet, Mainnet), PaymentKey, makeShelleyKeyWitness, writeFileTextEnvelope, TextEnvelopeDescr, HasTextEnvelope, readTextEnvelopeOfTypeFromFile, deserialiseFromTextEnvelope, serialiseToBech32, SerialiseAsBech32)
import Cardano.CLI.Shelley.Commands (WitnessFile(WitnessFile))
import Cardano.CLI.Environment ( EnvSocketError(..))
import Cardano.Api.LocalChainSync ( getLocalTip )
import Ouroboros.Network.Block (Tip, getTipPoint, getTipSlotNo)
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..), Either(QueryResultSuccess, QueryResultEraMismatch), Query(QueryIfCurrentShelley), EraMismatch (..))
import Ouroboros.Consensus.Shelley.Ledger.Query (Query(..))
import Ouroboros.Network.Point (fromWithOrigin)
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate (Either (DegenQueryResult),
                     Query (DegenQuery))
import qualified Shelley.Spec.Ledger.Address as Ledger
import qualified Shelley.Spec.Ledger.UTxO as Ledger
import qualified Shelley.Spec.Ledger.Tx as Ledger
import qualified Shelley.Spec.Ledger.Coin as Ledger
import           Shelley.Spec.Ledger.PParams (PParams)
import qualified Shelley.Spec.Ledger.PParams as Shelley
import           Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery (AcquireFailure (..))

import qualified Cardano.Crypto.Hash.Class as Crypto

import qualified Cardano.CLI.Shelley.Key as Key


import CLI.Jormungandr
import CLI.Interop (stripTrailingNewlines)
import Encoding
import Cardano.API.Voting (VotingKeyPublic, AsType(AsVotingKeyPublic))
import Cardano.API.Voting as Voting

import Turtle (ExitCode(ExitSuccess, ExitFailure), Shell, Line, procStrictWithErr, textToLines, select, inproc, shell, stdin)

data Bech32HumanReadablePartError = Bech32HumanReadablePartError !(Bech32.HumanReadablePartError)
  deriving Show

data AddressUTxOError = AddressHasNoUTxOs (Address Shelley)
  deriving Show

makeClassyPrisms ''Bech32DecodeError
makeClassyPrisms ''Bech32HumanReadablePartError
makeClassyPrisms ''AddressUTxOError
makeClassyPrisms ''TextViewError

liftExceptT :: (MonadError e' m, MonadIO m) => (e -> e') -> ExceptT e IO a -> m a
liftExceptT handler action = do
  result <- liftIO $ runExceptT action
  case result of
    Left err -> throwError $ handler err
    Right x  -> pure x


-- TODO Rename "LEDGER" to "SHELLEY" to match existing style in cardano-api
fromShelleyTxIn  :: Ledger.TxIn StandardShelley -> TxIn
fromShelleyTxIn (Ledger.TxIn txid txix) =
    TxIn (fromShelleyTxId txid) (TxIx (fromIntegral txix))

fromShelleyTxId :: Ledger.TxId StandardShelley -> TxId
fromShelleyTxId (Ledger.TxId h) =
    TxId (Crypto.castHash h)

data AppError
  = AppEnvSocketError !EnvSocketError
  | AppShelleyQueryError !ShelleyQueryCmdLocalStateQueryError
  | AppDecodeError !DecodeError
  | AppBech32DecodeError !Bech32DecodeError
  | AppBech32HumanReadablePartError !Bech32HumanReadablePartError
  | AppAddressUTxOError !AddressUTxOError
  | AppWriteTxError !(FileError ())
  deriving (Show)

makeClassyPrisms ''AppError

instance AsFileError AppError () where
  __FileError = _AppWriteTxError

instance AsEnvSocketError AppError where
  _EnvSocketError = _AppEnvSocketError

instance AsShelleyQueryCmdLocalStateQueryError AppError where
  _ShelleyQueryCmdLocalStateQueryError = _AppShelleyQueryError

instance AsDecodeError AppError where
  __DecodeError = _AppDecodeError
  
instance AsBech32DecodeError AppError where
  _Bech32DecodeError = _AppBech32DecodeError

instance AsBech32HumanReadablePartError AppError where
  __Bech32HumanReadablePartError = _AppBech32HumanReadablePartError

instance AsAddressUTxOError AppError where
  _AddressUTxOError = _AppAddressUTxOError

all
  :: ( MonadIO m
     , MonadError e m
     , MonadFail m
     , AsEnvSocketError e
     , AsShelleyQueryCmdLocalStateQueryError e
     , AsDecodeError e
     , AsBech32DecodeError e
     , AsBech32HumanReadablePartError e
     , AsAddressUTxOError e
     )
  => Protocol
  -> NetworkId
  -> Address Shelley
  -> SigningKey StakeKey 
  -> SigningKey PaymentKey
  -> VotingKeyPublic 
  -> m (Tx Shelley)
all protocol network addr stkSign psk votepk = do
  SocketPath sockPath <- readEnvSocketPath
  liftIO $ putStrLn $ show sockPath
  withlocalNodeConnectInfo protocol network sockPath $ \connectInfo -> do
    utxos   <- queryUTxOFromLocalState (FilterByAddress $ Set.singleton addr) connectInfo
    (txins, (Lovelace txOutValue)) <- case M.assocs $ Ledger.unUTxO utxos of
      []                                             -> throwError $ (_AddressHasNoUTxOs # addr)
      (txin, (Ledger.TxOut _ (Ledger.Coin value))):_ -> pure $ ([fromShelleyTxIn txin], (Lovelace value))
    let txoutsNoFee = [TxOut addr (Lovelace txOutValue)]
      
    let stkVerify = getVerificationKey stkSign
    
    liftIO $ putStrLn $ "Before meta"
    meta <- generateVoteMetadata stkSign stkVerify votepk
    liftIO $ putStrLn $ show meta

    tip     <- liftIO $ getLocalTip connectInfo
    let
      ttl    = fromWithOrigin minBound $ getTipSlotNo tip
      txBody = makeShelleyTransaction
                 txExtraContentEmpty {
                   txCertificates   = [],
                   txWithdrawals    = [],
                   txMetadata       = Just meta,
                   txUpdateProposal = Nothing
                 }
                 ttl
                 (toEnum 0) 
                 txins
                 txoutsNoFee
      tx = makeSignedTransaction [] txBody

    -- TODO Bundle this into one, estimateTransactionFee :: ConnectInfo -> Tx -> ...
    pparams <- queryPParamsFromLocalState connectInfo
    let
      (Lovelace fee) = estimateTransactionFee (localNodeNetworkId connectInfo) (Shelley._minfeeB pparams) (Shelley._minfeeB pparams) tx (length txins) (length txoutsNoFee) 0 0
      txoutsWithFee = [TxOut addr (Lovelace $ txOutValue - fee)]

      txBodyWithFee = makeShelleyTransaction
                        txExtraContentEmpty {
                          txCertificates   = [],
                          txWithdrawals    = [],
                          txMetadata       = Just meta,
                          txUpdateProposal = Nothing
                        }
                        (ttl + 5000)
                        (Lovelace fee) 
                        txins
                        txoutsWithFee
      witness   = makeShelleyKeyWitness txBodyWithFee (WitnessPaymentKey psk)
      txWithFee = makeSignedTransaction [witness] txBodyWithFee

    pure txWithFee

  --   pure utxos

generateVoteMetadata
  :: ( MonadIO m
     , MonadError e m
     , AsDecodeError e
     , AsBech32DecodeError e
     , AsBech32HumanReadablePartError e
     )
  => SigningKey StakeKey
  -> VerificationKey StakeKey
  -> VotingKeyPublic
  -> m TxMetadata
generateVoteMetadata stkSign stkVerify votepub = do
  jcliStkSign   <- jcliKeyFromBytes (serialiseToRawBytesHex stkSign) 
  hexSig        <- bech32SignatureToHex =<< jcliSign jcliStkSign votepub
  let stkVerifyHex = serialiseToRawBytes stkVerify

  x <- newPrefix "ed25519_pk" stkVerifyHex
  y <- newPrefix "ed25519_sig" hexSig
  jcliValidateSig x y votepub

  pure $ makeTransactionMetadata $ M.fromList [(1, TxMetaMap
                          [ ( TxMetaText "purpose"    , TxMetaText "voting_registration" )
                          , ( TxMetaText "voting_key" , TxMetaBytes $ serialiseToRawBytes votepub )
                          , ( TxMetaText "stake_pub"  , TxMetaBytes $ stkVerifyHex                )
                          , ( TxMetaText "signature"  , TxMetaBytes $ hexSig                      )
                          ]
                        )
                       ]

newPrefix hrPartTxt x =
  case Bech32.humanReadablePartFromText hrPartTxt of
    Left bech32HrPartErr -> throwError $ (_Bech32HumanReadablePartError #) bech32HrPartErr
    Right hrPart -> do
      pure $ Bech32.encodeLenient hrPart (Bech32.dataPartFromBytes x)

bech32SignatureToHex
  :: ( MonadError e m
     , AsBech32DecodeError e
     )
  => Text
  -> m BS.ByteString
bech32SignatureToHex sig =
  case Bech32.decodeLenient sig of
    Left err -> throwError (_Bech32DecodingError # err)
    Right (_, dataPart) ->
      case Bech32.dataPartToBytes dataPart of
        Nothing    -> throwError $ (_Bech32DataPartToBytesError # sig) 
        Just bytes -> pure bytes

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
  either (throwError . (_Bech32DecodeError #)) pure $ Voting.deserialiseFromBech32 AsVotingKeyPublic publicKeyBech32

readWitnessFile
  :: ( MonadIO m
     , MonadError e m
     , AsFileError e fileError
     , AsTextViewError fileError
     )
  => WitnessFile
  -> m (Witness Shelley)
readWitnessFile (WitnessFile fp) = do
  result <- liftIO $ readFileTextEnvelope AsShelleyWitness fp
  case result of
    Left (FileIOError fp e) -> throwError $ _FileIOError # (fp, e)
    Left (FileError fp e)   -> throwError $ _FileError   # (fp, _TextViewError # e)
    Right witness           -> pure witness

writeFileTextEnvelope'
  :: ( MonadIO m
     , MonadError e m
     , AsFileError e ()
     , HasTextEnvelope a
     )
  => FilePath
  -> Maybe TextEnvelopeDescr
  -> a
  -> m ()
writeFileTextEnvelope' fp mDesc x = do
  result <- liftIO $ writeFileTextEnvelope fp mDesc x
  case result of
    Left err -> throwError $ __FileError # err
    Right ()  -> pure ()

rTest fp = do
  eEnvelope <- readTextEnvelopeOfTypeFromFile (TextViewType "TxSignedShelley") fp
  case eEnvelope of
    Left err       -> error $ show err
    Right envelope -> case deserialiseFromTextEnvelope AsShelleyTx envelope of
      Left err -> error $ show err
      Right tx -> pure tx

