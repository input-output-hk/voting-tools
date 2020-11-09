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

import Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, left, newExceptT, handleIOExceptT)
import Control.Monad.Except (ExceptT, throwError, MonadError, runExceptT)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Set (Set)
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

import Cardano.Api.TextView (TextViewError)
import qualified Codec.Binary.Bech32 as Bech32
import Cardano.CLI.Shelley.Key (InputDecodeError, readSigningKeyFileAnyOf)
import Cardano.CLI.Shelley.Run.Key (SomeSigningKey(..))
import Cardano.CLI.Shelley.Run.Query (ShelleyQueryCmdError)
import Cardano.CLI.Types (SigningKeyFile (..), VerificationKeyFile (..), SocketPath(SocketPath), QueryFilter(NoFilter, FilterByAddress))
import Cardano.Api.Typed (FileError(FileError, FileIOError), LocalNodeConnectInfo(..), FromSomeType(FromSomeType), AsType(..), Key, SigningKey, NetworkId, VerificationKey, StandardShelley, NodeConsensusMode(ByronMode, ShelleyMode, CardanoMode), Address(ShelleyAddress, ByronAddress), Shelley, TxMetadataValue(TxMetaMap, TxMetaNumber, TxMetaText), txCertificates, txWithdrawals, txMetadata, txUpdateProposal, TxOut(TxOut), TxId(TxId), TxIx(TxIx))
import Cardano.Api.Protocol (withlocalNodeConnectInfo, Protocol(..))
import Cardano.API (queryNodeLocalState, getVerificationKey, StakeKey, serialiseToRawBytes, serialiseToRawBytesHex, deserialiseFromBech32, TxMetadata, Bech32DecodeError, makeTransactionMetadata, makeShelleyTransaction, txExtraContentEmpty, Lovelace(Lovelace), TxIn(TxIn), estimateTransactionFee, makeSignedTransaction, Witness, Tx, deserialiseAddress, TextEnvelopeError, readFileTextEnvelope)
import Cardano.CLI.Shelley.Commands (WitnessFile(WitnessFile))
import Cardano.CLI.Environment ( EnvSocketError(..) , readEnvSocketPath)
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

import CLI.Jormungandr
import Encoding

data Bech32HumanReadablePartError = Bech32HumanReadablePartError !(Bech32.HumanReadablePartError)

-- | An error that can occur while querying a node's local state.
data ShelleyQueryCmdLocalStateQueryError
  = AcquireFailureError !LocalStateQuery.AcquireFailure
  | EraMismatchError !EraMismatch
  -- ^ A query from a certain era was applied to a ledger from a different
  -- era.
  | ByronProtocolNotSupportedError
  -- ^ The query does not support the Byron protocol.
  deriving (Eq, Show)

data NotStakeSigningKeyError = NotStakeSigningKey !SomeSigningKey

data AddressUTxOError = AddressHasNoUTxOs (Address Shelley)

makeClassyPrisms ''FileError
makeClassyPrisms ''Bech32DecodeError
makeClassyPrisms ''Bech32HumanReadablePartError
makeClassyPrisms ''EnvSocketError
makeClassyPrisms ''ShelleyQueryCmdLocalStateQueryError
makeClassyPrisms ''NotStakeSigningKeyError
makeClassyPrisms ''AddressUTxOError
makeClassyPrisms ''InputDecodeError
makeClassyPrisms ''TextViewError

readSigningKeyFile
  :: ( MonadIO m
     , MonadError e m
     , AsFileError e fileErr
     , AsInputDecodeError fileErr
     )
  => SigningKeyFile
  -> m SomeSigningKey
readSigningKeyFile skFile = do
    result <- liftIO $ readSigningKeyFileAnyOf bech32FileTypes textEnvFileTypes skFile
    case result of
      Right x                 -> pure x
      Left (FileError fp e)   -> throwError (_FileError # (fp , _InputDecodeError # e))
      Left (FileIOError fp e) -> throwError (_FileIOError # (fp, e))
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

all
  :: ( MonadIO m
     , MonadError e m
     , MonadFail m
     , AsEnvSocketError e
     , AsShelleyQueryCmdLocalStateQueryError e
     , AsFileError e InputDecodeError
     , AsNotStakeSigningKeyError e
     , AsDecodeError e
     , AsBech32DecodeError e
     , AsBech32HumanReadablePartError e
     , AsAddressUTxOError e
     )
  => Protocol
  -> NetworkId
  -> Address Shelley
  -> SigningKeyFile
  -> Witness Shelley
  -> FilePath
  -> m (Tx Shelley)
all protocol network addr skf psk votekf = do
  SocketPath sockPath <- liftExceptT (_EnvSocketError #) $ readEnvSocketPath
  withlocalNodeConnectInfo protocol network sockPath $ \connectInfo -> do
    tip     <- liftIO $ getLocalTip connectInfo
    utxos   <- queryUTxOFromLocalState connectInfo tip (FilterByAddress $ Set.singleton addr)
    (txins, (Lovelace txOutValue)) <- case M.assocs $ Ledger.unUTxO utxos of
      []                                             -> throwError $ (_AddressHasNoUTxOs # addr)
      (txin, (Ledger.TxOut _ (Ledger.Coin value))):_ -> pure $ ([fromShelleyTxIn txin], (Lovelace value))
    let txoutsNoFee = [TxOut addr (Lovelace txOutValue)]
      
    stkSign <- readStakeSigningKey skf
    let stkVerify = getVerificationKey stkSign
    
    meta      <- generateVoteMetadata stkSign stkVerify votekf

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
    pparams <- queryPParamsFromLocalState connectInfo tip
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

      txWithFee = makeSignedTransaction [psk] txBodyWithFee

    pure txWithFee

  --   pure utxos

-- TODO Refactor query functions, they're very similar! queryShelley function?
-- TODO Ask why these functions aren't exposed
-- TODO Ask why using explicit errors and not classy errors?
queryPParamsFromLocalState
  :: ( MonadIO m
     , MonadError e m
     , AsShelleyQueryCmdLocalStateQueryError e
     -- TODO ask why MonadFail required?
     , MonadFail m
     )
  => LocalNodeConnectInfo mode block
  -> Tip block
  -> m (PParams StandardShelley)
queryPParamsFromLocalState LocalNodeConnectInfo{localNodeConsensusMode = ByronMode{}} _ =
  throwError (_ByronProtocolNotSupportedError # ())
queryPParamsFromLocalState connectInfo@LocalNodeConnectInfo{localNodeConsensusMode = ShelleyMode} tip = do
  DegenQueryResult result <- do
    x <- liftIO $ queryNodeLocalState connectInfo (getTipPoint tip, DegenQuery GetCurrentPParams)
    either (throwError . (_AcquireFailureError #)) pure x
  return result
queryPParamsFromLocalState connectInfo@LocalNodeConnectInfo{localNodeConsensusMode = CardanoMode{}} tip = do
  result <- do
    x <- liftIO $ queryNodeLocalState connectInfo (getTipPoint tip, QueryIfCurrentShelley GetCurrentPParams)
    either (throwError . (_AcquireFailureError #)) pure x
  case result of
    QueryResultEraMismatch eraerr -> throwError (_EraMismatchError # eraerr)
    QueryResultSuccess     pparams -> return pparams


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

readStakeSigningKey
  :: ( MonadIO m
     , MonadError e m
     , AsFileError e fileErr
     , AsInputDecodeError fileErr
     , AsNotStakeSigningKeyError e
     )
  => SigningKeyFile
  -> m (SigningKey StakeKey)
readStakeSigningKey skf = do
  ssk       <- readSigningKeyFile skf
  case ssk of
    AStakeSigningKey sk -> pure sk
    sk                  -> throwError $ (_NotStakeSigningKey # sk)

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

queryUTxOFromLocalState
  :: ( MonadIO m
     , MonadError e m
     , AsShelleyQueryCmdLocalStateQueryError e
     , MonadFail m
     )
  => LocalNodeConnectInfo mode block
  -> Tip block
  -> QueryFilter
  -> m (Ledger.UTxO StandardShelley)
queryUTxOFromLocalState connectInfo@LocalNodeConnectInfo{localNodeConsensusMode} tip qFilter =
  case localNodeConsensusMode of
    ByronMode{} -> throwError (_ByronProtocolNotSupportedError # ())

    ShelleyMode{} -> do
      DegenQueryResult result <- do
        x <- liftIO $ queryNodeLocalState connectInfo (getTipPoint tip, DegenQuery (applyUTxOFilter qFilter))
        either (throwError . (_AcquireFailureError #)) pure x
      return result

    CardanoMode{} -> do
      result <- do
        x <- liftIO $ queryNodeLocalState connectInfo (getTipPoint tip, QueryIfCurrentShelley (applyUTxOFilter qFilter))
        either (throwError . (_AcquireFailureError #)) pure x
      case result of
        QueryResultEraMismatch err -> throwError (_EraMismatchError # err)
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


parseAddress :: Atto.Parser (Address Shelley)
parseAddress = do
    str <- lexPlausibleAddressString
    case deserialiseAddress AsShelleyAddress str of
      Nothing   -> fail "invalid address"
      Just addr -> pure addr

lexPlausibleAddressString :: Atto.Parser Text
lexPlausibleAddressString =
    T.decodeLatin1 <$> Atto.takeWhile1 isPlausibleAddressChar
  where
    -- Covers both base58 and bech32 (with constrained prefixes)
    isPlausibleAddressChar c =
         (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      || (c >= '0' && c <= '9')
      || c == '_'

readerFromAttoParser :: Atto.Parser a -> Opt.ReadM a
readerFromAttoParser p =
    Opt.eitherReader (Atto.parseOnly (p <* Atto.endOfInput) . BSC.pack)
