module Main where

import qualified Options.Applicative as Opt

import Config

main :: IO ()
main = do
  options <- Opt.execParser opts
  putStrLn $ show options
  -- case opts of

  --   Left err ->
  --     putStrLn $ show err
  --   Right cmd -> do
  --     putStrLn $ show cmd
