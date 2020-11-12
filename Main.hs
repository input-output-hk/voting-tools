{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Options.Applicative as Opt
import Control.Monad.Except (runExceptT, ExceptT, throwError)

import Cardano.Api.Typed (Shelley, Tx, writeFileTextEnvelope)
import Cardano.Api.Protocol (Protocol(CardanoProtocol))
import Cardano.CLI.Types (SocketPath(SocketPath))
import Cardano.Chain.Slotting (EpochSlots (..))

import Control.Lens ((#))

import Config
import qualified Extern


main :: IO ()
main = do
  opts <- Opt.execParser opts
  eCfg  <- runExceptT $ mkConfig opts
  case eCfg of
    Left err  -> putStrLn $ show err
    Right (Config paymentAddr signStk signPay keyVotePublic networkId) -> do
      eResult <- runExceptT $ do
        tx <- Extern.all (CardanoProtocol $ EpochSlots 21600) networkId paymentAddr signStk signPay keyVotePublic :: ExceptT Extern.AppError IO (Tx Shelley)
        Extern.writeFileTextEnvelope' "./meta.txsigned" Nothing tx
      case eResult of
        Left err  -> putStrLn $ show err
        Right ()  -> pure ()

      
