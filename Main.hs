{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Options.Applicative as Opt
import Control.Monad.Except (runExceptT, ExceptT)

import Cardano.Api.Typed (Shelley, Tx)
import Cardano.Api.Protocol (Protocol(ShelleyProtocol))
import Cardano.CLI.Types (SocketPath(SocketPath))

import Config
import qualified Extern

main :: IO ()
main = do
  opts <- Opt.execParser opts
  eCfg  <- runExceptT $ mkConfig opts
  case eCfg of
    Left err  -> putStrLn $ show err
    Right (Config paymentAddr signStk signPay keyVotePublic networkId) -> do
      eResult <- runExceptT $ (Extern.all ShelleyProtocol networkId paymentAddr signStk signPay keyVotePublic :: ExceptT Extern.AppError IO (Tx Shelley))
      case eResult of
        Left err  -> putStrLn $ show err
        Right res -> putStrLn $ show res

      
