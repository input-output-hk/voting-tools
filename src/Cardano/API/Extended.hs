-- | Cardano.API.Extended.Raw but I've made the errors "classy". Plus
-- some utility functions.

{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.API.Extended ( queryUTxOFromLocalState
                            , queryPParamsFromLocalState
                            , Extended.ShelleyQueryCmdLocalStateQueryError(..)
                            , AsShelleyQueryCmdLocalStateQueryError(..)
                            , readSigningKeyFile
                            , AsFileError(..)
                            , AsInputDecodeError(..)
                            , AsEnvSocketError(..)
                            , Extended.readerFromAttoParser
                            , Extended.parseAddress
                            , Extended.pNetworkId
                            , readEnvSocketPath
                            , Extended.textEnvelopeToJSON
                            ) where

import Control.Monad.Trans.Except.Extra (firstExceptT, left, right, newExceptT)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, ExceptT, throwError, runExceptT)
import Control.Lens ((#))
import Control.Lens.TH (makeClassyPrisms)

import Cardano.API (HasTextEnvelope, SerialiseAsBech32, SigningKey, AsType)
import Cardano.Api.Typed (FileError(FileError, FileIOError))
import Cardano.Api.Typed (StandardShelley, LocalNodeConnectInfo(LocalNodeConnectInfo), localNodeConsensusMode, NodeConsensusMode(ByronMode, ShelleyMode, CardanoMode), Address, Shelley, FileError)
import Cardano.CLI.Environment ( EnvSocketError(..))
import qualified Cardano.CLI.Environment as Cardano (readEnvSocketPath) 
import Cardano.CLI.Shelley.Key (InputDecodeError)
import Cardano.CLI.Types (SigningKeyFile, QueryFilter, SocketPath)
import Ouroboros.Network.Block (Tip)
import Shelley.Spec.Ledger.PParams (PParams)
import qualified Cardano.CLI.Shelley.Key as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Ledger

import qualified Cardano.API.Extended.Raw as Extended


makeClassyPrisms ''Extended.ShelleyQueryCmdLocalStateQueryError
makeClassyPrisms ''FileError
makeClassyPrisms ''InputDecodeError
makeClassyPrisms ''EnvSocketError

liftExceptTIO
  :: ( MonadIO m
     , MonadError e' m
     )
  => (e -> e') -> ExceptT e IO a -> m a
liftExceptTIO f exc = do
  x <- liftIO $ runExceptT exc
  either (throwError . f) pure x

queryUTxOFromLocalState
  :: ( MonadIO m
     , MonadError e m
     , AsShelleyQueryCmdLocalStateQueryError e
     )
  => QueryFilter
  -> LocalNodeConnectInfo mode block
  -> m (Ledger.UTxO StandardShelley)
queryUTxOFromLocalState qf =
  liftExceptTIO (_ShelleyQueryCmdLocalStateQueryError #) .
    Extended.queryUTxOFromLocalState qf

queryPParamsFromLocalState
  :: ( MonadIO m
     , MonadError e m
     , AsShelleyQueryCmdLocalStateQueryError e
     )
  => LocalNodeConnectInfo mode block
  -> m (PParams StandardShelley)
queryPParamsFromLocalState = 
  liftExceptTIO (_ShelleyQueryCmdLocalStateQueryError #) .
    Extended.queryPParamsFromLocalState

readSigningKeyFile
  :: forall e m fileErr keyrole.
     ( MonadIO m
     , MonadError e m
     , AsFileError e fileErr
     , AsInputDecodeError fileErr
     , HasTextEnvelope (SigningKey keyrole)
     , SerialiseAsBech32 (SigningKey keyrole)
     )
  => AsType keyrole
  -> SigningKeyFile
  -> m (SigningKey keyrole)
readSigningKeyFile role f = do
  result <- liftIO $ Shelley.readSigningKeyFile role f
  case result of
    Right x                 -> pure x
    Left (FileError fp e)   -> throwError (_FileError # (fp , _InputDecodeError # e))
    Left (FileIOError fp e) -> throwError (_FileIOError # (fp, e))

readEnvSocketPath
  :: ( MonadIO m
     , MonadError e m
     , AsEnvSocketError e
     )
  => m SocketPath
readEnvSocketPath =
  liftExceptTIO (_EnvSocketError #)
    Cardano.readEnvSocketPath
