-- | Parts of the cardano-api that I need exposed but which aren't so
-- I've replicated them here.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.API.Extended.Raw where

import           Control.Applicative ((<|>))
import Data.Foldable (asum)
import           Control.Lens (( # ))
import           Data.Word (Word64)
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

import           Cardano.Api
import           Cardano.Api.Modes (AnyConsensusModeParams(..), ConsensusModeParams(..), EpochSlots(EpochSlots))
import           Cardano.Api (AsType (AsShelleyAddress), HasTextEnvelope,
                     NetworkId (Mainnet, Testnet), TextEnvelopeDescr, deserialiseAddress,
                     queryNodeLocalState, serialiseToTextEnvelope)
import Cardano.Api.Protocol (Protocol(ByronProtocol, ShelleyProtocol, CardanoProtocol))
import           Cardano.Api.LocalChainSync (getLocalTip)
import           Cardano.Api.Shelley
import           Cardano.Api.Typed (LocalNodeConnectInfo, NetworkMagic (NetworkMagic),
                     ShelleyLedgerEra)
import           Cardano.Api.Typed (Address (ByronAddress, ShelleyAddress),
                     LocalNodeConnectInfo (LocalNodeConnectInfo),
                     NodeConsensusMode (ByronMode, CardanoMode, ShelleyMode), Shelley,
                     StandardShelley, localNodeConsensusMode)
import           Cardano.CLI.Types (QueryFilter (FilterByAddress, NoFilter))
import           Ouroboros.Consensus.Cardano.Block
                     (Either (QueryResultEraMismatch, QueryResultSuccess), EraMismatch (..),
                     Query (QueryIfCurrentShelley))
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate (Either (DegenQueryResult),
                     Query (DegenQuery))
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import           Ouroboros.Consensus.Shelley.Ledger.Query
                     (Query (GetCurrentPParams, GetFilteredUTxO, GetUTxO))
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)
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

parseAddressAny :: Atto.Parser AddressAny
parseAddressAny = do
    str <- lexPlausibleAddressString
    case deserialiseAddress AsAddressAny str of
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

-- | Select the appropriate query constructor based on the era
-- 'QueryIfCurrentShelley', 'QueryIfCurrentAllegra' or 'QueryIfCurrentMary'.
--
--
queryIfCurrentEra :: ShelleyBasedEra era
                  -> Query (Consensus.ShelleyBlock (ShelleyLedgerEra era)) result
                  -> Consensus.CardanoQuery StandardCrypto
                       (Consensus.CardanoQueryResult StandardCrypto result)
queryIfCurrentEra ShelleyBasedEraShelley = Consensus.QueryIfCurrentShelley
queryIfCurrentEra ShelleyBasedEraAllegra = Consensus.QueryIfCurrentAllegra
queryIfCurrentEra ShelleyBasedEraMary    = Consensus.QueryIfCurrentMary

defaultByronEpochSlots :: Word64
defaultByronEpochSlots = 21600

pConsensusModeParams :: Opt.Parser AnyConsensusModeParams
pConsensusModeParams = asum
  [ Opt.flag' (AnyConsensusModeParams ShelleyModeParams)
      (  Opt.long "shelley-mode"
      <> Opt.help "For talking to a node running in Shelley-only mode."
      )
  , Opt.flag' ()
      (  Opt.long "byron-mode"
      <> Opt.help "For talking to a node running in Byron-only mode."
      )
       *> pByronConsensusMode
  , Opt.flag' ()
      (  Opt.long "cardano-mode"
      <> Opt.help "For talking to a node running in full Cardano mode (default)."
      )
       *> pCardanoConsensusMode
  , -- Default to the Cardano consensus mode.
    pure . AnyConsensusModeParams . CardanoModeParams $ EpochSlots defaultByronEpochSlots
  ]
 where
   pCardanoConsensusMode :: Opt.Parser AnyConsensusModeParams
   pCardanoConsensusMode = AnyConsensusModeParams . CardanoModeParams <$> pEpochSlots
   pByronConsensusMode :: Opt.Parser AnyConsensusModeParams
   pByronConsensusMode = AnyConsensusModeParams . ByronModeParams <$> pEpochSlots

pEpochSlots :: Opt.Parser EpochSlots
pEpochSlots =
  EpochSlots <$>
    Opt.option Opt.auto
      (  Opt.long "epoch-slots"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The number of slots per epoch for the Byron era."
      <> Opt.value defaultByronEpochSlots -- Default to the mainnet value.
      <> Opt.showDefault
      )

