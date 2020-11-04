{-# LANGUAGE OverloadedStrings #-}

module CLI.Jormungandr where

import Data.Text (Text)
import Data.Text as T
import Turtle (ExitCode(ExitSuccess, ExitFailure), Shell, Line, procStrictWithErr)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Cont (runContT)

import CLI.Interop (withTextAsFile)

jcliCmd :: MonadIO io => [Text] -> Shell Line -> io Text
jcliCmd args stdIn = do
  (exitCode, stdOut, stdErr) <- procStrictWithErr "jcli" args stdIn
  case exitCode of
    ExitSuccess      -> pure stdOut
    ExitFailure code -> error $
      "Encountered exit code " <> show code
      <> " while executing \"jcli " <> T.unpack (T.intercalate " " args) <> "\".\n"
      <> "stderr was: " <> T.unpack stdErr <> "\n"
      <> "stdout was: " <> T.unpack stdOut

jcliSign :: String -> String -> IO Text
jcliSign stake_sk vote_pk_bytes = (`runContT` pure) $ do
  skFile     <- withTextAsFile stake_sk
  votePkFile <- withTextAsFile vote_pk_bytes
  liftIO $ jcliCmd ["key", "sign", "--secret-key", T.pack skFile, T.pack votePkFile] mempty
