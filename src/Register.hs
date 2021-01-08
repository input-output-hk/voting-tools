{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Cardano.API (ShelleyBasedEra (ShelleyBasedEraShelley))
import           Cardano.Api.LocalChainSync (getLocalTip)
import           Cardano.Api.Protocol (Protocol (CardanoProtocol), withlocalNodeConnectInfo)
import           Cardano.Api.Typed (Lovelace (Lovelace), Shelley, Tx, TxId (TxId), TxIn (TxIn),
                     TxIx (TxIx), getVerificationKey, localNodeNetworkId, serialiseToRawBytesHex,
                     writeFileTextEnvelope)
import qualified Cardano.Binary as CBOR
import           Cardano.Chain.Slotting (EpochSlots (..))
import           Cardano.CLI.Types (QueryFilter (FilterByAddress), SocketPath (SocketPath))
import qualified Cardano.Crypto.DSIGN as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Set as Set
import qualified Options.Applicative as Opt
import           Ouroboros.Network.Block (Tip, getTipPoint, getTipSlotNo)
import           Ouroboros.Network.Point (fromWithOrigin)

import           Cardano.API.Extended (readEnvSocketPath)
import           Cardano.CLI.Voting (createVote, encodeVote, prettyTx, signTx)
import           Cardano.CLI.Voting.Error
import           Cardano.CLI.Voting.Metadata (voteSignature)
import           Cardano.CLI.Voting.Signing (verificationKeyRawBytes)
import           Config.Registration

main :: IO ()
main = do
  -- Parse command-line options
  opts <- Opt.execParser opts
  eCfg  <- runExceptT $ mkConfig opts
  case eCfg of
    Left err  -> putStrLn $ show err
    Right (Config addr voteSign paySign votePub networkId ttl outFile) -> do
      eResult <- runExceptT $ do
        SocketPath sockPath <-  readEnvSocketPath
        withlocalNodeConnectInfo (CardanoProtocol $ EpochSlots 21600) networkId sockPath $ \connectInfo -> do
          -- Create a transaction, encoding our vote as transaction
          -- metadata. The transaction sends some unspent ADA back to us
          -- (minus a fee).

          -- Generate vote payload (vote information is encoded as metadata).
          let vote = createVote voteSign votePub

          -- Encode the vote as a transaction and sign it
          voteTx <- signTx paySign <$> encodeVote connectInfo ShelleyBasedEraShelley addr ttl vote

          -- Output helpful information
          liftIO . putStrLn $ "Vote public key used        (hex): " <> BSC.unpack (serialiseToRawBytesHex votePub)
          liftIO . putStrLn $ "Stake public key used       (hex): " <> BSC.unpack (verificationKeyRawBytes voteSign)
          liftIO . putStrLn $ "Vote registration signature (hex): " <> BSC.unpack (Base16.encode . Crypto.rawSerialiseSigDSIGN $ voteSignature vote)

          -- Output our vote transaction
          liftIO . writeFile outFile $ prettyTx voteTx
      case eResult of
        Left  (err :: AppError) -> error $ show err
        Right ()                -> pure ()


