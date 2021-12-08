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
import           Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty', keyOrder)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (asum)
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Word (Word64)
import qualified Options.Applicative as Opt

import           Cardano.Api (AddressAny, AnyConsensusModeParams (..),
                   AsType (AsAddressAny, AsStakeAddress), ConsensusModeParams (..), EpochSlots (..),
                   HasTextEnvelope, NetworkId (Mainnet, Testnet), NetworkMagic (..), StakeAddress,
                   TextEnvelopeDescr, deserialiseAddress, serialiseToTextEnvelope)
import           Cardano.Api.Shelley ()

parseAddressAny :: Atto.Parser AddressAny
parseAddressAny = do
    str <- lexPlausibleAddressString
    case deserialiseAddress AsAddressAny str of
      Nothing   -> fail "invalid address"
      Just addr -> pure addr

parseStakeAddress :: Atto.Parser StakeAddress
parseStakeAddress = do
    str <- lexPlausibleAddressString
    case deserialiseAddress AsStakeAddress str of
      Nothing   -> fail "invalid address"
      Just addr -> pure addr

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

