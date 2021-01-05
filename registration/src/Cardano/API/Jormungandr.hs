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
import qualified Data.Binary.Strict.Get as Bin
import Data.Word

parseAddressBytes = do
  discrimBin.getWord8

-- data Discrimination = Mainnet
--                     | Testnet

-- data AddressKind = 

-- data Address = Address {
--   _addrNetwork :: Discrimination
