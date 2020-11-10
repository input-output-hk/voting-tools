{-# LANGUAGE TemplateHaskell #-}

module Encoding (DecodeError(DecodeError), decodeBytesUtf8, AsDecodeError(_DecodeError, __DecodeError)) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Error (OnDecodeError)
import Control.Exception.Safe
import Control.Exception (evaluate)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word8)
import Control.Lens ((#))
import Control.Lens.TH (makeClassyPrisms)
import System.IO.Unsafe (unsafeDupablePerformIO)

data DecodeError = DecodeError String (Maybe Word8)
  -- ^ String that describes the error and the input value that caused
  -- the error. If the error arose because the end of input was
  -- reached or could not be identified precisely, this value will be
  -- 'Nothing'.
  deriving (Eq, Show)

makeClassyPrisms ''DecodeError

instance Exception DecodeError

customStrictDecode :: OnDecodeError
customStrictDecode desc c = throw (DecodeError desc c)

decodeBytesUtf8
  :: ( MonadError e m
     , AsDecodeError e
     )
  => ByteString
  -> m Text
decodeBytesUtf8 bs = do
  let result = unsafeDupablePerformIO . try . evaluate . T.decodeUtf8With customStrictDecode $ bs
  case result of
    Left (DecodeError desc input) -> throwError $ (_DecodeError #) (desc, input)
    Right txt                     -> pure txt
