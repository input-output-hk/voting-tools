{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}

module Extern where

import Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, left, newExceptT)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Set (Set)
import qualified Data.Set as Set

import Cardano.CLI.Shelley.Key (InputDecodeError, readSigningKeyFileAnyOf)
import Cardano.CLI.Shelley.Run.Key (SomeSigningKey(..))
import Cardano.CLI.Shelley.Run.Query (ShelleyQueryCmdError)
import Cardano.CLI.Types (SigningKeyFile (..), VerificationKeyFile (..), SocketPath(SocketPath), QueryFilter(NoFilter, FilterByAddress))
import Cardano.Api.Typed (LocalNodeConnectInfo(..), FileError, FromSomeType(FromSomeType), AsType(..), Key, SigningKey, NetworkId, VerificationKey, StandardShelley, NodeConsensusMode(ByronMode, ShelleyMode, CardanoMode), Address(ShelleyAddress, ByronAddress), Shelley,)
import Cardano.Api.Protocol (withlocalNodeConnectInfo, Protocol(..))
import Cardano.API (queryNodeLocalState, getVerificationKey, StakeKey)
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

all :: Protocol -> NetworkId -> Set (Address Shelley) -> SigningKeyFile -> ExceptT VoterRegistrationError IO (Ledger.UTxO StandardShelley)
all protocol network as skf = do
  SocketPath sockPath <- firstExceptT VoterEnvSocketError readEnvSocketPath
  withlocalNodeConnectInfo protocol network sockPath $ \connectInfo -> do
    tip     <- liftIO $ getLocalTip connectInfo
    utxos   <- queryUTxOFromLocalState connectInfo tip (FilterByAddress as)
    stkSign <- readStakeSigningKey skf
    let stkVerify = getVerificationKey stkSign
    
    -- meta      <- generateMeta stkVerify votePkBytes hexSig

    pure utxos

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
