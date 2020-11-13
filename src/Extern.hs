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

import Cardano.Api.TextView (TextViewError, TextViewType(TextViewType))
import qualified Codec.Binary.Bech32 as Bech32
import Cardano.CLI.Shelley.Key (InputDecodeError, readSigningKeyFileAnyOf)
import Cardano.CLI.Shelley.Run.Key (SomeSigningKey(..))
import Cardano.CLI.Shelley.Run.Query (ShelleyQueryCmdError)
import Cardano.CLI.Types (SigningKeyFile (..), VerificationKeyFile (..), SocketPath(SocketPath), QueryFilter(NoFilter, FilterByAddress))
import Cardano.Api.Typed (FileError(FileError, FileIOError), LocalNodeConnectInfo(..), FromSomeType(FromSomeType), AsType(..), Key, SigningKey, NetworkId, VerificationKey, StandardShelley, NodeConsensusMode(ByronMode, ShelleyMode, CardanoMode), Address(ShelleyAddress, ByronAddress), Shelley, TxMetadataValue(TxMetaMap, TxMetaNumber, TxMetaText, TxMetaBytes), txCertificates, txWithdrawals, txMetadata, txUpdateProposal, TxOut(TxOut), TxId(TxId), TxIx(TxIx), NetworkMagic(NetworkMagic), ShelleyWitnessSigningKey(WitnessPaymentKey))
import Cardano.Api.Protocol (withlocalNodeConnectInfo, Protocol(..))
import Cardano.API (queryNodeLocalState, getVerificationKey, StakeKey, serialiseToRawBytes, serialiseToRawBytesHex, deserialiseFromBech32, TxMetadata, Bech32DecodeError, makeTransactionMetadata, makeShelleyTransaction, txExtraContentEmpty, Lovelace(Lovelace), TxIn(TxIn), estimateTransactionFee, makeSignedTransaction, Witness, Tx, deserialiseAddress, TextEnvelopeError, readFileTextEnvelope, NetworkMagic, NetworkId(Testnet, Mainnet), PaymentKey, makeShelleyKeyWitness, writeFileTextEnvelope, TextEnvelopeDescr, HasTextEnvelope, readTextEnvelopeOfTypeFromFile, deserialiseFromTextEnvelope)
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
import CLI.Interop (stripTrailingNewlines)
import Encoding
import Cardano.API.Voting (VotingKeyPublic, AsType(AsVotingKeyPublic))
import Cardano.API.Voting as Voting

import Turtle (ExitCode(ExitSuccess, ExitFailure), Shell, Line, procStrictWithErr, textToLines, select, inproc, shell, stdin)

data Bech32HumanReadablePartError = Bech32HumanReadablePartError !(Bech32.HumanReadablePartError)
  deriving Show

-- | An error that can occur while querying a node's local state.
data ShelleyQueryCmdLocalStateQueryError
  = AcquireFailureError !LocalStateQuery.AcquireFailure
  | EraMismatchError !EraMismatch
  -- ^ A query from a certain era was applied to a ledger from a different
  -- era.
  | ByronProtocolNotSupportedError
  -- ^ The query does not support the Byron protocol.
  deriving (Eq, Show)

data NotStakeSigningKeyError = NotStakeSigningKey !SigningKeyFile
  deriving Show

data NotPaymentSigningKeyError = NotPaymentSigningKey !SigningKeyFile
  deriving Show

data AddressUTxOError = AddressHasNoUTxOs (Address Shelley)
  deriving Show

makeClassyPrisms ''FileError
makeClassyPrisms ''Bech32DecodeError
makeClassyPrisms ''Bech32HumanReadablePartError
makeClassyPrisms ''EnvSocketError
makeClassyPrisms ''ShelleyQueryCmdLocalStateQueryError
makeClassyPrisms ''NotStakeSigningKeyError
makeClassyPrisms ''NotPaymentSigningKeyError
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

data AppError
  = AppEnvSocketError !EnvSocketError
  | AppShelleyQueryError !ShelleyQueryCmdLocalStateQueryError
  | AppDecodeError !DecodeError
  | AppBech32DecodeError !Bech32DecodeError
  | AppBech32HumanReadablePartError !Bech32HumanReadablePartError
  | AppAddressUTxOError !AddressUTxOError
  | AppWriteTxError !(FileError ())
  deriving Show

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
  SocketPath sockPath <- liftExceptT (_EnvSocketError #) $ readEnvSocketPath
  liftIO $ putStrLn $ show sockPath
  withlocalNodeConnectInfo protocol network sockPath $ \connectInfo -> do
    liftIO $ putStrLn $ "Before tip"
    tip     <- liftIO $ getLocalTip connectInfo
    utxos   <- queryUTxOFromLocalState connectInfo tip (FilterByAddress $ Set.singleton addr)
    (txins, (Lovelace txOutValue)) <- case M.assocs $ Ledger.unUTxO utxos of
      []                                             -> throwError $ (_AddressHasNoUTxOs # addr)
      (txin, (Ledger.TxOut _ (Ledger.Coin value))):_ -> pure $ ([fromShelleyTxIn txin], (Lovelace value))
    let txoutsNoFee = [TxOut addr (Lovelace txOutValue)]
      
    let stkVerify = getVerificationKey stkSign
    
    liftIO $ putStrLn $ "Before meta"
    meta <- generateVoteMetadata stkSign stkVerify votepk
    liftIO $ putStrLn $ show meta

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
      witness   = makeShelleyKeyWitness txBodyWithFee (WitnessPaymentKey psk)
      txWithFee = makeSignedTransaction [witness] txBodyWithFee

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
     , AsDecodeError e
     , AsBech32DecodeError e
     , AsBech32HumanReadablePartError e
     )
  => SigningKey StakeKey
  -> VerificationKey StakeKey
  -> VotingKeyPublic
  -> m TxMetadata
generateVoteMetadata stkSign stkVerify votepub = do
  liftIO $ putStrLn "meta start"
  -- votepubBytes   <- jcliKeyToBytes votepub
  -- liftIO $ TIO.putStr votepubBytes
  liftIO $ putStrLn $ show stkSign
  liftIO $ putStrLn $ show (serialiseToRawBytesHex stkSign)
  liftIO $ putStrLn "toBytes done..."
  jcliStkSign   <- jcliKeyFromBytes (serialiseToRawBytesHex stkSign) 
  liftIO $ putStrLn "fromBytes done..."
  liftIO $ putStrLn $ show jcliStkSign
  -- jcliStkPublic <- jcliKeyPublic jcliStkSign
  -- jcliStkPublic <- jcliKeyPublic "ed25519_sk1a7ea75d7u6zc4a6eyl8q7jpgu4v8dkxrzyvrr7ru5szck3f5s7f69hd"
  liftIO $ putStrLn "key public done..."
  sig           <- jcliSign jcliStkSign votepub
  liftIO $ putStrLn $ "key sign done..." <> show sig
  hexSig        <- bech32SignatureToHex sig
  liftIO $ putStrLn $ "decode done" <> show hexSig
  let stkVerifyHex = serialiseToRawBytes stkVerify

  liftIO $ putStrLn $ show "new prefixing... | " <> show stkVerifyHex <> " | " <> show hexSig
  x <- newPrefix "ed25519_pk" stkVerifyHex
  y <- newPrefix "ed25519_sig" hexSig
  liftIO $ putStrLn $ show x
  liftIO $ putStrLn $ show y
  liftIO $ putStrLn $ show "VALIDATING..."
  jcliValidateSig x y votepub
  liftIO $ putStrLn $ show "VALID"

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
    sk                  -> throwError $ (_NotStakeSigningKey # skf)

readPaymentSigningKey
  :: ( MonadIO m
     , MonadError e m
     , AsFileError e fileErr
     , AsInputDecodeError fileErr
     , AsNotPaymentSigningKeyError e
     )
  => SigningKeyFile
  -> m (SigningKey PaymentKey)
readPaymentSigningKey skf = do
  psk <- readSigningKeyFile skf
  case psk of
    APaymentSigningKey k -> pure k
    sk                   -> throwError $ (_NotPaymentSigningKey # skf)

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

pNetworkId :: Opt.Parser NetworkId
pNetworkId =
  pMainnet' <|> fmap Testnet pTestnetMagic
 where
   pMainnet' :: Opt.Parser NetworkId
   pMainnet' =
    Opt.flag' Mainnet
      (  Opt.long "mainnet"
      <> Opt.help "Use the mainnet magic id."
      )

pTestnetMagic :: Opt.Parser NetworkMagic
pTestnetMagic =
  NetworkMagic <$>
    Opt.option Opt.auto
      (  Opt.long "testnet-magic"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Specify a testnet magic id."
      )

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

