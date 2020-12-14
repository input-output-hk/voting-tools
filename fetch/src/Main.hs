{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Options.Applicative as Opt

import           Config

main :: IO ()
main = do
  -- Parse command-line options
  opts <- Opt.execParser opts
  putStrLn $ show opts
  -- eCfg  <- runExceptT $ mkConfig opts
  -- case eCfg of
  --   Left err  -> putStrLn $ show err
  --   Right (Config networkId threshold dbCfg slot extraFunds) -> do
  --     SocketPath sockPath <-  readEnvSocketPath
  --     withlocalNodeConnectInfo (CardanoProtocol $ EpochSlots 21600) networkId sockPath $ \connectInfo -> do
        
