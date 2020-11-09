{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Options.Applicative as Opt
import Control.Monad.Except (runExceptT)

import Config

main :: IO ()
main = do
  opts <- Opt.execParser opts
  eCfg  <- runExceptT $ mkConfig opts
  case eCfg of
    Left err  -> putStrLn $ show err
    Right cfg -> putStrLn $ show cfg
