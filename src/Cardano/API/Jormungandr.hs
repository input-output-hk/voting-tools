{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JormungandrAddress
--
--  Copied from: https://github.com/input-output-hk/chain-libs/blob/f10cea331ff62dcda9d60963d49dd104c624235c/chain-addr/src/lib.rs
--
-- It uses a simple serialization format which is made to be concise:
--
-- * First byte contains the discrimination information (1 bit) and the kind of address (7 bits)
-- * Remaining bytes contains a kind specific encoding describe after.
--
-- 4 kinds of address are currently supported:
--
-- * Single: Just a (spending) public key using the ED25519 algorithm
-- * Group: Same as single, but with a added (staking/group) public key
--   using the ED25519 algorithm.
-- * Account: A account public key using the ED25519 algorithm
-- * Multisig: a multisig account public key
--
-- Single key:
--     DISCRIMINATION_BIT || SINGLE_KIND_TYPE (7 bits) || SPENDING_KEY
--
-- Group key:
--     DISCRIMINATION_BIT || GROUP_KIND_TYPE (7 bits) || SPENDING_KEY || ACCOUNT_KEY
--
-- Account key:
--     DISCRIMINATION_BIT || ACCOUNT_KIND_TYPE (7 bits) || ACCOUNT_KEY
--
-- Multisig key:
--     DISCRIMINATION_BIT || MULTISIG_KIND_TYPE (7 bits) || MULTISIG_MERKLE_ROOT_PUBLIC_KEY
--
-- Script identifier:
--     DISCRIMINATION_BIT || SCRIPT_KIND_TYPE (7 bits) || SCRIPT_IDENTIFIER
--
-- Address human format is bech32 encoded
--

module Cardano.API.Jormungandr where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin
import Data.Word
import Cardano.Api.Typed (NetworkId(Mainnet, Testnet), AsType, HasTypeProxy (proxyToAsType), SerialiseAsRawBytes (deserialiseFromRawBytes, serialiseToRawBytes))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Bits ( (.&.) )

import Cardano.API.Extended (serialiseToBech32', SerialiseAsBech32'(bech32PrefixFor, bech32PrefixesPermitted), VotingKeyPublic, deserialiseFromBech32')

data Discrimination = Production
                    | Test

-- Wrapping type so we don't have orphan instances.
data Address = Address { unAddress :: B.ByteString }
  deriving (Eq, Show, Ord)

addressFromVotingKeyPublic :: NetworkId -> VotingKeyPublic -> Address
addressFromVotingKeyPublic nw =
  mkSingleAddress (discriminationFromNetworkId nw) . serialiseToRawBytes

discriminationFromNetworkId :: NetworkId -> Discrimination
discriminationFromNetworkId Mainnet          = Production
discriminationFromNetworkId (Testnet _magic) = Test

mkSingleAddress :: Discrimination -> B.ByteString -> Address
mkSingleAddress d bs = Address . B.concat $ BL.toChunks . Bin.runPut $ do
  case d of
    Production -> Bin.putWord8 (0b01111111 .&. 0x3)
    Test       -> Bin.putWord8 (0b11111111 .&. 0x3)
  Bin.putByteString bs

instance SerialiseAsBech32' Address where
    bech32PrefixFor (Address _) = "ca"
    bech32PrefixesPermitted AsAddress = ["ca"]

instance HasTypeProxy Address where
  data AsType Address = AsAddress
  proxyToAsType _ = AsAddress

instance SerialiseAsRawBytes Address where
  serialiseToRawBytes (Address raw) = raw
  deserialiseFromRawBytes AsAddress = Just . Address

instance FromJSON Address where
  parseJSON = Aeson.withText "Address" $ \t ->
    case (deserialiseFromBech32' AsAddress t) of
      Left err    -> fail $ "Failed to deserialise Address from Bech32 string: " <> show t <> "\n"
        <> "Error was: " <> show err
      Right addr  -> pure addr

instance ToJSON Address where
  toJSON = Aeson.String . serialiseToBech32'

getAddress :: Bin.Get Address
getAddress = do
    -- We use 'lookAhead' to not consume the header, and let it
    -- be included in the underlying Address ByteString.
    headerByte <- Bin.label "address header" . Bin.lookAhead $ Bin.getWord8
    let kind = kindValue headerByte
    case kind of
        0x3 -> Address <$> Bin.getByteString 33 -- single address
        0x4 -> Address <$> Bin.getByteString 65 -- grouped address
        0x5 -> Address <$> Bin.getByteString 33 -- account address
        0x6 -> Address <$> Bin.getByteString 33 -- multisig address
        other -> fail $ "Invalid address type: " ++ show other
  where
    kindValue :: Word8 -> Word8
    kindValue = (.&. 0b01111111)

putAddress :: Address -> Bin.Put
putAddress (Address bs) = Bin.putByteString bs
