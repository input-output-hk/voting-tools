{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Options.Applicative as Opt
import Control.Monad.Except (runExceptT, ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Cardano.Api.LocalChainSync ( getLocalTip )
import Ouroboros.Network.Block (Tip, getTipPoint, getTipSlotNo)

import Cardano.Api.Typed (Shelley, Tx, writeFileTextEnvelope, localNodeNetworkId, Lovelace(Lovelace), TxIn(TxIn), TxId(TxId), TxIx(TxIx))
import Cardano.Api.Protocol (Protocol(CardanoProtocol), withlocalNodeConnectInfo)
import Cardano.API.Misc
import Cardano.CLI.Types (SocketPath(SocketPath), QueryFilter(FilterByAddress))
import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.API.Misc (queryPParamsFromLocalState, readEnvSocketPath)
import Ouroboros.Network.Point (fromWithOrigin)
import qualified Data.Set as Set
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Binary as CBOR

import Control.Lens ((#))

import Config
import Extern

main :: IO ()
main = do
  -- Parse command-line options
  opts <- Opt.execParser opts
  eCfg  <- runExceptT $ mkConfig opts
  case eCfg of
    Left err  -> putStrLn $ show err
    Right (Config addr stkSign paySign votePub networkId ttl) -> do
      eResult <- runExceptT $ do
        SocketPath sockPath <- readEnvSocketPath
        withlocalNodeConnectInfo (CardanoProtocol $ EpochSlots 21600) networkId sockPath $ \connectInfo -> do
          -- Create a transaction, encoding our vote as transaction
          -- metadata. The transaction sends some unspent ADA back to us
          -- (minus a fee).

          -- Generate vote payload (vote information is encoded as metadata).
          meta                   <- generateVoteMetadata stkSign votePub

          tip                    <- liftIO $ getLocalTip connectInfo

          -- Estimate the fee for the transaction
          pparams                <- queryPParamsFromLocalState connectInfo
          let
            networkId  = localNodeNetworkId connectInfo
            slotTip    = fromWithOrigin minBound $ getTipSlotNo tip
            -- Start with a base estimate
            feeBase    = estimateVoteTxFee networkId pparams slotTip [] addr (Lovelace 0) meta
            -- Estimate the fee per extra txin
            mockTxIn   = TxIn (TxId $ Crypto.hashWith CBOR.serialize' ()) (TxIx 1)
            feePerTxIn =
              estimateVoteTxFee networkId pparams slotTip [mockTxIn] addr (Lovelace 0) meta
              - feeBase

          -- Find some unspent funds
          utxos <- queryUTxOFromLocalState (FilterByAddress $ Set.singleton addr) connectInfo

          case unspentCoveringFees feeBase feePerTxIn (fromShelleyUTxO utxos) of
            (feeReached, Nothing) -> undefined
            (_, Just unspent)     -> do
              let
                txins = unspentSources unspent 
                value = unspentValue   unspent
                fee   =
                  estimateVoteTxFee
                    networkId pparams slotTip txins addr value meta

              -- Create and sign the vote transaction
                txBody = voteTx addr txins value (slotTip + ttl) fee meta
                tx     = signTx paySign txBody

              -- Output our transaction
              writeFileTextEnvelope' "./meta.txsigned" Nothing tx
      case eResult of
        Left  (err :: AppError) -> error $ show err
        Right ()                -> pure ()
