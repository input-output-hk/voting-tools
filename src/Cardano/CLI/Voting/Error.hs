{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Application error type and instances.

module Cardano.CLI.Voting.Error where

import           Cardano.Api (AddressAny, Bech32DecodeError, FileError, Lovelace, TextEnvelopeError)
import           Cardano.CLI.Environment (EnvSocketError (..))
import           Control.Lens.TH (makeClassyPrisms)

import           Cardano.API.Extended (AsBech32DecodeError (_Bech32DecodeError),
                   AsBech32HumanReadablePartError (__Bech32HumanReadablePartError),
                   AsEnvSocketError (_EnvSocketError), AsFileError (__FileError),
                   Bech32HumanReadablePartError)
import           Cardano.CLI.Voting.Fee (AsNotEnoughFundsError (_NotEnoughFundsError),
                   NotEnoughFundsError)

-- | Address doesn't have enough UTxOs to pay the requested amount.
data AddressUTxOError = AddressNotEnoughUTxOs AddressAny Lovelace
    deriving Show

makeClassyPrisms ''AddressUTxOError
makeClassyPrisms ''TextEnvelopeError

data AppError = AppEnvSocketError !EnvSocketError
    | AppBech32DecodeError !Bech32DecodeError
    | AppBech32HumanReadablePartError !Bech32HumanReadablePartError
    | AppAddressUTxOError !AddressUTxOError
    | AppWriteTxError !(FileError ())
    | AppNotEnoughFundsError !NotEnoughFundsError
    deriving (Show)

makeClassyPrisms ''AppError

instance AsFileError AppError () where
  __FileError = _AppWriteTxError

instance AsEnvSocketError AppError where
  _EnvSocketError = _AppEnvSocketError

instance AsBech32DecodeError AppError where
  _Bech32DecodeError = _AppBech32DecodeError

instance AsBech32HumanReadablePartError AppError where
  __Bech32HumanReadablePartError = _AppBech32HumanReadablePartError

instance AsAddressUTxOError AppError where
  _AddressUTxOError = _AppAddressUTxOError

instance AsNotEnoughFundsError AppError where
  _NotEnoughFundsError = _AppNotEnoughFundsError
