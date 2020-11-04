{-# LANGUAGE RankNTypes #-}

module Extern where

-- import Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, left, newExceptT)

-- import Cardano.CLI.Shelley.Key (InputDecodeError, readSigningKeyFileAnyOf)
-- import Cardano.CLI.Shelley.Run.Key (SomeSigningKey)
-- import Cardano.CLI.Types (SigningKeyFile (..), VerificationKeyFile (..))
-- import Cardano.Api.Typed (LocalNodeConnectInfo(..))
-- import Cardano.Api.Protocol (withlocalNodeConnectInfo, Protocol(..))
-- import Cardano.CLI.Environment ( EnvSocketError(..) , readEnvSocketPath)
-- import Cardano.Api.LocalChainSync ( getLocalTip )
-- import Ouroboros.Network.Block (Tip)
-- import qualified Shelley.Spec.Ledger.Address as Ledger

-- readSigningKeyFile
--   :: SigningKeyFile
--   -> ExceptT (FileError InputDecodeError) IO SomeSigningKey
-- readSigningKeyFile skFile =
--     newExceptT $
--       readSigningKeyFileAnyOf bech32FileTypes textEnvFileTypes skFile
--   where
--     textEnvFileTypes =
--       [ FromSomeType (AsSigningKey AsByronKey)
--                       AByronSigningKey
--       , FromSomeType (AsSigningKey AsPaymentKey)
--                       APaymentSigningKey
--       , FromSomeType (AsSigningKey AsPaymentExtendedKey)
--                       APaymentExtendedSigningKey
--       , FromSomeType (AsSigningKey AsStakeKey)
--                       AStakeSigningKey
--       , FromSomeType (AsSigningKey AsStakeExtendedKey)
--                       AStakeExtendedSigningKey
--       , FromSomeType (AsSigningKey AsStakePoolKey)
--                       AStakePoolSigningKey
--       , FromSomeType (AsSigningKey AsGenesisKey)
--                       AGenesisSigningKey
--       , FromSomeType (AsSigningKey AsGenesisExtendedKey)
--                       AGenesisExtendedSigningKey
--       , FromSomeType (AsSigningKey AsGenesisDelegateKey)
--                       AGenesisDelegateSigningKey
--       , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey)
--                       AGenesisDelegateExtendedSigningKey
--       , FromSomeType (AsSigningKey AsGenesisUTxOKey)
--                       AGenesisUTxOSigningKey
--       , FromSomeType (AsSigningKey AsVrfKey)
--                       AVrfSigningKey
--       , FromSomeType (AsSigningKey AsKesKey)
--                       AKesSigningKey
--       ]

--     bech32FileTypes =
--       [ FromSomeType (AsSigningKey AsByronKey)
--                       AByronSigningKey
--       , FromSomeType (AsSigningKey AsPaymentKey)
--                       APaymentSigningKey
--       , FromSomeType (AsSigningKey AsPaymentExtendedKey)
--                       APaymentExtendedSigningKey
--       , FromSomeType (AsSigningKey AsStakeKey)
--                       AStakeSigningKey
--       , FromSomeType (AsSigningKey AsStakeExtendedKey)
--                       AStakeExtendedSigningKey
--       , FromSomeType (AsSigningKey AsStakePoolKey)
--                       AStakePoolSigningKey
--       , FromSomeType (AsSigningKey AsVrfKey)
--                       AVrfSigningKey
--       , FromSomeType (AsSigningKey AsKesKey)
--                       AKesSigningKey
--       ]

-- withSomeSigningKey :: SomeSigningKey
--                    -> (forall keyrole. Key keyrole => SigningKey keyrole -> a)
--                    -> a
-- withSomeSigningKey ssk f =
--     case ssk of
--       AByronSigningKey           sk -> f sk
--       APaymentSigningKey         sk -> f sk
--       APaymentExtendedSigningKey sk -> f sk
--       AStakeSigningKey           sk -> f sk
--       AStakeExtendedSigningKey   sk -> f sk
--       AStakePoolSigningKey       sk -> f sk
--       AGenesisSigningKey         sk -> f sk
--       AGenesisExtendedSigningKey sk -> f sk
--       AGenesisDelegateSigningKey sk -> f sk
--       AGenesisDelegateExtendedSigningKey
--                                  sk -> f sk
--       AGenesisUTxOSigningKey     sk -> f sk
--       AVrfSigningKey             sk -> f sk
--       AKesSigningKey             sk -> f sk

-- queryTip
--   :: Protocol
--   -> NetworkId
--   -> ExceptT ShelleyQueryCmdError IO Tip
-- queryTip protocol network = do
--     SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
--     output <-
--       firstExceptT ShelleyQueryCmdLocalStateQueryError $
--       withlocalNodeConnectInfo protocol network sockPath $ \connectInfo -> do
--         liftIO $ getLocalTip connectInfo

-- getVerificationKey :: SigningKeyFile
--                    -> ExceptT ShelleyKeyCmdError IO VerificationKey
-- getVerificationKey skf = do
--     ssk <- firstExceptT ShelleyKeyCmdReadKeyFileError $ readSigningKeyFile skf
--     withSomeSigningKey ssk $ \sk ->
--       pure $ getVerificationKey sk

-- runQueryUTxO
--   :: Protocol
--   -> QueryFilter
--   -> NetworkId
--   -> ExceptT ShelleyQueryCmdError IO (Ledger.UTxO StandardShelley)
-- runQueryUTxO protocol qfilter network mOutFile = do
--   SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
--   filteredUtxo <- firstExceptT ShelleyQueryCmdLocalStateQueryError $
--     withlocalNodeConnectInfo protocol network sockPath (queryUTxOFromLocalState qfilter)

-- queryUTxOFromLocalState
--   :: QueryFilter
--   -> LocalNodeConnectInfo mode block
--   -> ExceptT ShelleyQueryCmdLocalStateQueryError IO (Ledger.UTxO StandardShelley)
-- queryUTxOFromLocalState qFilter connectInfo@LocalNodeConnectInfo{localNodeConsensusMode} =
--   case localNodeConsensusMode of
--     ByronMode{} -> throwError ByronProtocolNotSupportedError

--     ShelleyMode{} -> do
--       tip <- liftIO $ getLocalTip connectInfo
--       DegenQueryResult result <- firstExceptT AcquireFailureError . newExceptT $
--         queryNodeLocalState
--           connectInfo
--           (getTipPoint tip, DegenQuery (applyUTxOFilter qFilter))
--       return result

--     CardanoMode{} -> do
--       tip <- liftIO $ getLocalTip connectInfo
--       result <- firstExceptT AcquireFailureError . newExceptT $
--         queryNodeLocalState
--           connectInfo
--           (getTipPoint tip, QueryIfCurrentShelley (applyUTxOFilter qFilter))
--       case result of
--         QueryResultEraMismatch err -> throwError (EraMismatchError err)
--         QueryResultSuccess utxo -> return utxo
--   where
--     applyUTxOFilter (FilterByAddress as) = GetFilteredUTxO (toShelleyAddrs as)
--     applyUTxOFilter NoFilter             = GetUTxO

--     -- TODO: ultimately, these should be exported from Cardano.API.Shelley
--     -- for the Shelley-specific types and conversion for the API wrapper types.
--     -- But alternatively, the API can also be extended to cover the queries
--     -- properly using the API types.

--     toShelleyAddrs :: Set (Address Shelley) -> Set (Ledger.Addr StandardShelley)
--     toShelleyAddrs = Set.map toShelleyAddr

--     toShelleyAddr :: Address era -> Ledger.Addr StandardShelley
--     toShelleyAddr (ByronAddress addr)        = Ledger.AddrBootstrap
--                                                  (Ledger.BootstrapAddress addr)
--     toShelleyAddr (ShelleyAddress nw pc scr) = Ledger.Addr nw pc scr


-- -- cardano.build_tx("meta.txbody", txins=txins, txouts=txouts, ttl=tip["slotNo"], metadata = meta)
-- -- fee = cardano.estimate_fee(len(txins), len(txouts), 1, txbody_file="meta.txbody")
-- -- txouts = [( arguments["--payment-address"], value - fee )]
-- -- cardano.build_tx("meta.txbody", txins=txins, txouts=txouts, ttl=tip["slotNo"] + 5000, metadata=meta, fee=fee)
-- -- cardano.sign_tx("meta.txbody", "meta.txsigned", [ arguments["--payment-signing-key"] ])
