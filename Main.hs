{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Options.Applicative as Opt

import Config (opts)

-- data Config
--   = Config { networkId             :: NetworkId
--            , queryFilter           :: QueryFilter -- this includes the "payment address"
--            , protocol              :: Protocol

--            , stateDir              :: FilePath
--            , paymentSigningKeyFile :: FilePath
--            -- , paymentAddress        :: String -- TODO use cardano-api Address
--            , votePublicKeyFile     :: FilePath
--            , stakeSigningKeyFile   :: SigningKeyFile
--            }

main :: IO ()
main = do
  opts <- Opt.execParser opts
  putStrLn $ show opts
