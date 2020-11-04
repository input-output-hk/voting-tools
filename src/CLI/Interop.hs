
module CLI.Interop (withTmpFile, withTextAsFile) where

import System.IO as Sys
import Control.Exception.Safe as E
import System.Directory as Dir
import Control.Monad.Cont (ContT(ContT), liftIO)

-- | Open a temporary file for the caller and provide them with the
-- file path and handle. Closes the handle and deletes the file
-- afterwards.
withTmpFile :: ContT r IO (FilePath, Handle)
withTmpFile = ContT $ \action -> do
  tmpDir <- Dir.getTemporaryDirectory `catchIOError` (\_ -> pure ".")
  E.bracket (Sys.openTempFile tmpDir "tmp")
            cleanup
            action
  where
    cleanup (tmpFile,tmpHandle) = do
        Sys.hClose tmpHandle
        Dir.removeFile tmpFile

-- | Serializes the given string to a temporary file and provides the
-- file to the caller. Destroys the file afterwards.
--
-- Useful if you have some text but you need to shell out to an
-- external program that only accepts files.
withTextAsFile :: String -> ContT r IO FilePath
withTextAsFile content = do
  (filePath, handle) <- withTmpFile
  liftIO $ Sys.hPutStr handle content
  pure filePath
