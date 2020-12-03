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
import Cardano.CLI.Types (SocketPath(SocketPath), QueryFilter(FilterByAddress))
import Cardano.Chain.Slotting (EpochSlots (..))
import qualified Data.ByteString.Char8 as BSC
import Cardano.API.Extended (readEnvSocketPath)
import Cardano.CLI.Voting (createVote, signTx, encodeVote, prettyTx)
import Cardano.CLI.Voting.Error
import Ouroboros.Network.Point (fromWithOrigin)
import qualified Data.Set as Set
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Binary as CBOR

import Control.Lens ((#))

import Config

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
          let vote = createVote stkSign votePub

          -- Encode the vote as a transaction and sign it
          voteTx <- signTx paySign <$> encodeVote connectInfo addr ttl vote

          -- Output our vote transaction
          liftIO . putStr . prettyTx $ voteTx
      case eResult of
        Left  (err :: AppError) -> error $ show err
        Right ()                -> pure ()


