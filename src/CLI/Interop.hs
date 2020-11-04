
module CLI.Interop (withTmpFile) where

import Control.Safe.Exception as E

withTmpFile :: _
withTmpFile action = do
  tmpDir <- getTemporaryDirectory `catchIOError` (\_ -> return ".")
  E.bracket (openTempFile tmpDir "tmp")
            cleanup
            action
  where
    cleanup (tmpFile,tmpHandle) = do
        hClose tmpHandle
        removeFile tmpFile
