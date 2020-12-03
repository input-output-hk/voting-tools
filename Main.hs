{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Cardano.Api.LocalChainSync (getLocalTip)
import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as Opt
import           Ouroboros.Network.Block (Tip, getTipPoint, getTipSlotNo)

import           Cardano.API.Extended (readEnvSocketPath)
import           Cardano.Api.Protocol (Protocol (CardanoProtocol), withlocalNodeConnectInfo)
import           Cardano.Api.Typed (Lovelace (Lovelace), Shelley, Tx, TxId (TxId), TxIn (TxIn),
                     TxIx (TxIx), localNodeNetworkId, writeFileTextEnvelope)
import qualified Cardano.Binary as CBOR
import           Cardano.Chain.Slotting (EpochSlots (..))
import           Cardano.CLI.Types (QueryFilter (FilterByAddress), SocketPath (SocketPath))
import           Cardano.CLI.Voting (createVote, encodeVote, prettyTx, signTx)
import           Cardano.CLI.Voting.Error
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Set as Set
import           Ouroboros.Network.Point (fromWithOrigin)

import           Control.Lens (( # ))

import           Config

main :: IO ()
main = do
  -- Parse command-line options
  opts <- Opt.execParser opts
  eCfg  <- runExceptT $ mkConfig opts
  case eCfg of
    Left err  -> putStrLn $ show err
    Right (Config addr stkSign paySign votePub networkId ttl) -> do
      eResult <- runExceptT $ do
        SocketPath sockPath <-  readEnvSocketPath
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


