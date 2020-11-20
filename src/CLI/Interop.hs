{-# LANGUAGE OverloadedStrings #-}

module CLI.Interop (withTmpFile, withTextAsFile, withBinaryAsFile, stripTrailingNewlines) where

import Data.Text (Text)
import qualified Data.Text as T
import System.IO as Sys
import Control.Exception.Safe as E
import System.Directory as Dir
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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
  liftIO $ Sys.hClose handle
  pure filePath

-- | Serializes the given byte string to a temporary file and provides
-- the file to the caller. Destroys the file afterwards.
--
-- Useful if you have some binary data but you need to shell out to an
-- external program that only accepts files.
withBinaryAsFile :: ByteString -> ContT r IO FilePath
withBinaryAsFile content = do
  (filePath, handle) <- withTmpFile
  liftIO $ BS.hPutStr handle content
  liftIO $ Sys.hClose handle
  pure filePath

stripTrailingNewlines :: Text -> Text
stripTrailingNewlines = T.intercalate "\n" . filter (not . T.null) . T.lines
