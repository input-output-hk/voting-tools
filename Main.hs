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

import Cardano.Api.Typed (Shelley, Tx, writeFileTextEnvelope, localNodeNetworkId)
import Cardano.Api.Protocol (Protocol(CardanoProtocol), withlocalNodeConnectInfo)
import Cardano.CLI.Types (SocketPath(SocketPath))
import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.API.Misc (queryPParamsFromLocalState, readEnvSocketPath)
import Ouroboros.Network.Point (fromWithOrigin)

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
    Right (Config addr stkSign paySign votePub networkId) -> do
      eResult <- runExceptT $ do
        SocketPath sockPath <- readEnvSocketPath
        withlocalNodeConnectInfo (CardanoProtocol $ EpochSlots 21600) networkId sockPath $ \connectInfo -> do
          -- Create a transaction, encoding our vote as transaction
          -- metadata. The transaction sends some unspent ADA back to us
          -- (minus a fee).

          -- Generate vote payload (vote information is encoded as metadata).
          meta                   <- generateVoteMetadata stkSign votePub

          -- Find some unspent funds
          unspent@(txins, value) <- findUnspentTx connectInfo addr

          tip                    <- liftIO $ getLocalTip connectInfo

          -- Estimate the fee for the transaction
          pparams                <- queryPParamsFromLocalState connectInfo
          let
            networkId = localNodeNetworkId connectInfo
            slotTip   = fromWithOrigin minBound $ getTipSlotNo tip
            fee       = estimateVoteTxFee networkId pparams slotTip txins addr value meta

          -- Create and sign the vote transaction
          let
            txBody = voteTx addr unspent (slotTip + 5000) fee meta
            tx     = signTx paySign txBody

          -- Output our transaction
          writeFileTextEnvelope' "./meta.txsigned" Nothing tx
      case eResult of
        Left  (err :: AppError) -> error $ show err
        Right ()                -> pure ()
