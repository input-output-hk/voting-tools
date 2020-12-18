{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Application error type and instances.

module Cardano.CLI.Voting.Error where

import           Cardano.API (Address, Bech32DecodeError, FileError, Lovelace (Lovelace))
import           Cardano.Api.TextView (TextViewError)
import           Cardano.Api.Typed (Shelley)
import           Cardano.CLI.Environment (EnvSocketError (..))
import qualified Codec.Binary.Bech32 as Bech32
import           Control.Lens.TH (makeClassyPrisms)

import           Cardano.API.Extended (AsBech32DecodeError (_Bech32DecodeError),
                     AsBech32HumanReadablePartError (__Bech32HumanReadablePartError),
                     AsEnvSocketError (_EnvSocketError), AsFileError (__FileError),
                     AsShelleyQueryCmdLocalStateQueryError (_ShelleyQueryCmdLocalStateQueryError),
                     Bech32HumanReadablePartError, ShelleyQueryCmdLocalStateQueryError)
import           Cardano.CLI.Voting.Fee (AsNotEnoughFundsError (_NotEnoughFundsError),
                     NotEnoughFundsError)

-- | Address doesn't have enough UTxOs to pay the requested amount.
data AddressUTxOError = AddressNotEnoughUTxOs (Address Shelley) Lovelace
    deriving Show

makeClassyPrisms ''AddressUTxOError
makeClassyPrisms ''TextViewError

data AppError = AppEnvSocketError !EnvSocketError
    | AppShelleyQueryError !ShelleyQueryCmdLocalStateQueryError
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

instance AsShelleyQueryCmdLocalStateQueryError AppError where
  _ShelleyQueryCmdLocalStateQueryError = _AppShelleyQueryError

instance AsBech32DecodeError AppError where
  _Bech32DecodeError = _AppBech32DecodeError

instance AsBech32HumanReadablePartError AppError where
  __Bech32HumanReadablePartError = _AppBech32HumanReadablePartError

instance AsAddressUTxOError AppError where
  _AddressUTxOError = _AppAddressUTxOError

instance AsNotEnoughFundsError AppError where
  _NotEnoughFundsError = _AppNotEnoughFundsError
