{-# LANGUAGE TemplateHaskell #-}

module Encoding ( DecodeError(DecodeError)
                , decodeBytesUtf8
                , AsDecodeError(_DecodeError, __DecodeError)
                , AsBech32DecodeError(..)
                , bech32SignatureToBytes
                , newPrefix
                , AsBech32HumanReadablePartError(..)
                , Bech32HumanReadablePartError(Bech32HumanReadablePartError)
                ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Error (OnDecodeError)
import Control.Exception.Safe
import Control.Exception (evaluate)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Codec.Binary.Bech32 as Bech32
import Data.Word (Word8)
import Control.Lens ((#))
import Control.Lens.TH (makeClassyPrisms)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Cardano.API (Bech32DecodeError)

data DecodeError = DecodeError String (Maybe Word8)
  -- ^ String that describes the error and the input value that caused
  -- the error. If the error arose because the end of input was
  -- reached or could not be identified precisely, this value will be
  -- 'Nothing'.
  deriving (Eq, Show)

makeClassyPrisms ''DecodeError

instance Exception DecodeError

makeClassyPrisms ''Bech32DecodeError

data Bech32HumanReadablePartError = Bech32HumanReadablePartError !(Bech32.HumanReadablePartError)
  deriving Show

makeClassyPrisms ''Bech32HumanReadablePartError

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

bech32SignatureToBytes
  :: ( MonadError e m
     , AsBech32DecodeError e
     )
  => Text
  -> m BS.ByteString
bech32SignatureToBytes sig =
  case Bech32.decodeLenient sig of
    Left err -> throwError (_Bech32DecodingError # err)
    Right (_, dataPart) ->
      case Bech32.dataPartToBytes dataPart of
        Nothing    -> throwError $ (_Bech32DataPartToBytesError # sig)
        Just bytes -> pure bytes

newPrefix
  :: ( MonadError e m
     , AsBech32HumanReadablePartError e
     )
  => Text
  -- ^ New prefix
  -> ByteString
  -- ^ Raw Bech32 bytes
  -> m Text
newPrefix hrPartTxt x =
  case Bech32.humanReadablePartFromText hrPartTxt of
    Left bech32HrPartErr -> throwError $ (_Bech32HumanReadablePartError #) bech32HrPartErr
    Right hrPart -> do
      pure $ Bech32.encodeLenient hrPart (Bech32.dataPartFromBytes x)
