-- | Parts of the cardano-api that I need exposed but which aren't so
-- I've replicated them here.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.API.Extended.Raw where

import           Control.Applicative ((<|>))
import           Control.Lens (( # ))
import           Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT, right)
import           Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty', keyOrder)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Options.Applicative as Opt

import           Cardano.API (AsType (AsShelleyAddress), HasTextEnvelope,
                     NetworkId (Mainnet, Testnet), TextEnvelopeDescr, deserialiseAddress,
                     queryNodeLocalState, serialiseToTextEnvelope)
import           Cardano.Api.LocalChainSync (getLocalTip)
import           Cardano.Api.Typed (LocalNodeConnectInfo, NetworkMagic (NetworkMagic))
import           Cardano.Api.Typed (Address (ByronAddress, ShelleyAddress),
                     LocalNodeConnectInfo (LocalNodeConnectInfo),
                     NodeConsensusMode (ByronMode, CardanoMode, ShelleyMode), Shelley,
                     StandardShelley, localNodeConsensusMode)
import           Cardano.CLI.Types (QueryFilter (FilterByAddress, NoFilter))
import           Ouroboros.Consensus.Cardano.Block
                     (Either (QueryResultEraMismatch, QueryResultSuccess), EraMismatch (..),
                     Query (QueryIfCurrentShelley))
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate (Either (DegenQueryResult),
                     Query (DegenQuery))
import           Ouroboros.Consensus.Shelley.Ledger.Query
                     (Query (GetCurrentPParams, GetFilteredUTxO, GetUTxO))
import           Ouroboros.Network.Block (Tip, getTipPoint, getTipSlotNo)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
                     (AcquireFailure (..))
import qualified Shelley.Spec.Ledger.Address as Ledger
import qualified Shelley.Spec.Ledger.Address as Ledger
import qualified Shelley.Spec.Ledger.Coin as Ledger
import           Shelley.Spec.Ledger.PParams (PParams)
import qualified Shelley.Spec.Ledger.PParams as Shelley
import qualified Shelley.Spec.Ledger.Tx as Ledger
import qualified Shelley.Spec.Ledger.UTxO as Ledger

-- | An error that can occur while querying a node's local state.
data ShelleyQueryCmdLocalStateQueryError = AcquireFailureError !LocalStateQuery.AcquireFailure
    | EraMismatchError !EraMismatch
    | ByronProtocolNotSupportedError
    deriving (Eq, Show)

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryUTxOFromLocalState
  :: QueryFilter
  -> LocalNodeConnectInfo mode block
  -> ExceptT ShelleyQueryCmdLocalStateQueryError IO (Ledger.UTxO StandardShelley)
queryUTxOFromLocalState qFilter connectInfo@LocalNodeConnectInfo{localNodeConsensusMode} =
  case localNodeConsensusMode of
    ByronMode{} -> throwError ByronProtocolNotSupportedError

    ShelleyMode{} -> do
      tip <- liftIO $ getLocalTip connectInfo
      DegenQueryResult result <- firstExceptT AcquireFailureError . newExceptT $
        queryNodeLocalState
          connectInfo
          (getTipPoint tip, DegenQuery (applyUTxOFilter qFilter))
      return result

    CardanoMode{} -> do
      tip <- liftIO $ getLocalTip connectInfo
      result <- firstExceptT AcquireFailureError . newExceptT $
        queryNodeLocalState
          connectInfo
          (getTipPoint tip, QueryIfCurrentShelley (applyUTxOFilter qFilter))
      case result of
        QueryResultEraMismatch err -> throwError (EraMismatchError err)
        QueryResultSuccess utxo    -> return utxo
  where
    applyUTxOFilter (FilterByAddress as) = GetFilteredUTxO (toShelleyAddrs as)
    applyUTxOFilter NoFilter             = GetUTxO

    -- TODO: ultimately, these should be exported from Cardano.API.Shelley
    -- for the Shelley-specific types and conversion for the API wrapper types.
    -- But alternatively, the API can also be extended to cover the queries
    -- properly using the API types.

    toShelleyAddrs :: Set (Address Shelley) -> Set (Ledger.Addr StandardShelley)
    toShelleyAddrs = Set.map toShelleyAddr

    toShelleyAddr :: Address era -> Ledger.Addr StandardShelley
    toShelleyAddr (ByronAddress addr)        = Ledger.AddrBootstrap
                                                 (Ledger.BootstrapAddress addr)
    toShelleyAddr (ShelleyAddress nw pc scr) = Ledger.Addr nw pc scr

-- | Query the current protocol parameters from a Shelley node via the local
-- state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryPParamsFromLocalState
  :: LocalNodeConnectInfo mode block
  -> ExceptT ShelleyQueryCmdLocalStateQueryError IO (PParams StandardShelley)
queryPParamsFromLocalState LocalNodeConnectInfo{
                             localNodeConsensusMode = ByronMode{}
                           } =
    throwError ByronProtocolNotSupportedError

queryPParamsFromLocalState connectInfo@LocalNodeConnectInfo{
                             localNodeConsensusMode = ShelleyMode
                           } = do
    tip <- liftIO $ getLocalTip connectInfo
    DegenQueryResult result <- firstExceptT AcquireFailureError . newExceptT $
      queryNodeLocalState
        connectInfo
        (getTipPoint tip, DegenQuery GetCurrentPParams)
    return result

queryPParamsFromLocalState connectInfo@LocalNodeConnectInfo{
                             localNodeConsensusMode = CardanoMode{}
                           } = do
    tip <- liftIO $ getLocalTip connectInfo
    result <- firstExceptT AcquireFailureError . newExceptT $
      queryNodeLocalState
        connectInfo
        (getTipPoint tip, QueryIfCurrentShelley GetCurrentPParams)
    case result of
      QueryResultEraMismatch eraerr  -> throwError (EraMismatchError eraerr)
      QueryResultSuccess     pparams -> return pparams

parseAddress :: Atto.Parser (Address Shelley)
parseAddress = do
    str <- lexPlausibleAddressString
    case deserialiseAddress AsShelleyAddress str of
      Nothing   -> fail "invalid address"
      Just addr -> pure addr

lexPlausibleAddressString :: Atto.Parser Text
lexPlausibleAddressString =
    T.decodeLatin1 <$> Atto.takeWhile1 isPlausibleAddressChar
  where
    -- Covers both base58 and bech32 (with constrained prefixes)
    isPlausibleAddressChar c =
         (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      || (c >= '0' && c <= '9')
      || c == '_'

readerFromAttoParser :: Atto.Parser a -> Opt.ReadM a
readerFromAttoParser p =
    Opt.eitherReader (Atto.parseOnly (p <* Atto.endOfInput) . BSC.pack)

pNetworkId :: Opt.Parser NetworkId
pNetworkId =
  pMainnet' <|> fmap Testnet pTestnetMagic
 where
   pMainnet' :: Opt.Parser NetworkId
   pMainnet' =
    Opt.flag' Mainnet
      (  Opt.long "mainnet"
      <> Opt.help "Use the mainnet magic id."
      )

pTestnetMagic :: Opt.Parser NetworkMagic
pTestnetMagic =
  NetworkMagic <$>
    Opt.option Opt.auto
      (  Opt.long "testnet-magic"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Specify a testnet magic id."
      )

textEnvelopeJSONConfig :: Config
textEnvelopeJSONConfig = defConfig { confCompare = textEnvelopeJSONKeyOrder }

textEnvelopeToJSON :: HasTextEnvelope a =>  Maybe TextEnvelopeDescr -> a -> BSC.ByteString
textEnvelopeToJSON mbDescr a  =
  LBS.toStrict $ encodePretty' textEnvelopeJSONConfig
                               (serialiseToTextEnvelope mbDescr a)
              <> "\n"

textEnvelopeJSONKeyOrder :: Text -> Text -> Ordering
textEnvelopeJSONKeyOrder = keyOrder ["type", "description", "cborHex"]
