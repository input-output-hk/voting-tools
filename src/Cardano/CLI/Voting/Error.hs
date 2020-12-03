{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.CLI.Voting.Error where

import Cardano.Api.TextView (TextViewError)
import Cardano.CLI.Environment ( EnvSocketError(..))
import Cardano.API (Bech32DecodeError, Address, Lovelace(Lovelace), FileError)
import Cardano.Api.Typed (Shelley)
import qualified Codec.Binary.Bech32 as Bech32
import Control.Lens.TH (makeClassyPrisms)

import Cardano.API.Extended (ShelleyQueryCmdLocalStateQueryError, AsFileError(__FileError), AsEnvSocketError(_EnvSocketError), AsShelleyQueryCmdLocalStateQueryError(_ShelleyQueryCmdLocalStateQueryError), AsBech32DecodeError(_Bech32DecodeError), Bech32HumanReadablePartError, AsBech32HumanReadablePartError(__Bech32HumanReadablePartError))

-- | Address doesn't have enough UTxOs to pay the requested amount.
data AddressUTxOError = AddressNotEnoughUTxOs (Address Shelley) Lovelace
  deriving Show

makeClassyPrisms ''AddressUTxOError
makeClassyPrisms ''TextViewError

data AppError
  = AppEnvSocketError !EnvSocketError
  | AppShelleyQueryError !ShelleyQueryCmdLocalStateQueryError
  | AppBech32DecodeError !Bech32DecodeError
  | AppBech32HumanReadablePartError !Bech32HumanReadablePartError
  | AppAddressUTxOError !AddressUTxOError
  | AppWriteTxError !(FileError ())
  deriving (Show)

makeClassyPrisms ''AppError

instance AsFileError AppError () where
  __FileError = _AppWriteTxError

instance AsEnvSocketError AppError where
  _EnvSocketError = _AppEnvSocketError

instance AsShelleyQueryCmdLocalStateQueryError AppError where
  _ShelleyQueryCmdLocalStateQueryError = _AppShelleyQueryError

instance AsBech32DecodeError AppError where
  _Bech32DecodeError = _AppBech32DecodeError

instance AsBech32HumanReadablePartError AppError where
  __Bech32HumanReadablePartError = _AppBech32HumanReadablePartError

instance AsAddressUTxOError AppError where
  _AddressUTxOError = _AppAddressUTxOError
