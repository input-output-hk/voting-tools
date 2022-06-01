{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{- |

Module      : Cardano.Catalyst.Test.DSL.Internal.Types
Description : Types for the testing DSL.
Maintainer  : sevanspowell
Stability   : experimental


__WARNING This is an internal module, it's interface is not safe. It is recommended to import "Cardano.Catalyst.Test.DSL" instead. If you must import this module, notify the maintainer of your use case so we can safely support it.__

This module provides the types necessary to construct terms in the "Test DSL".
It is recommended to use this DSL in conjunction with the
"Cardano.Catalyst.Test.DSL.Gen" module to generate appropriate terms.

For example:

@
fn = do
  (stakeRego :: StakeRegistration 'Ephemeral) <-
    genStakeAddressRegistration
  let stakeKey = stakeRegoKey stakeRego
  (rego :: Registration 'Ephemeral) <-
    (signed . setSlotNo 2) <$> genVoteRegistration stakeKey
  pure (stakeRego, rego)
@

Data types in this DSL follow the pattern of defining two variations of a type:

  - an 'Ephemeral' version (for data not committed to a database), and
  - a 'Persisted' version (for data written to a database).

The two variations have:

  - a common interface (for data common to both types), and
  - a specific interface (for data only available to one of the two variants).

This type-level encoding provides safety when manipulating data that may or may
not have been committed to a database.

Terms can be converted from the 'Ephemeral' to 'Persisted' by using the
"Cardano.Catalyst.Test.DSL.Internal.Db" module, which includes code for
persisting the data to a database.
-}

module Cardano.Catalyst.Test.DSL.Internal.Types
  ( -- * Types
    PersistState(..)
  -- ** Transaction
  , TransactionState(..)
  , Transaction(..)
  , setTransactionSlot
  , getTransactionSlot
  , getTxKey
  , getBlockKey
  , getSlotLeaderKey
  -- ** Registration
  , Registration(..)
  , setSlotNo
  , signed
  , unsigned
  , getRegistrationVote
  , getRegistrationVotePayload
  -- ** UTxO
  , UTxOState(..)
  , UTxO(..)
  , setUTxOSlot
  , setStakeAddressId
  , getStakeAddressId
  , utxoValue
  , getStakeAddress
  -- ** Stake Registration
  , StakeRegistrationState(..)
  , StakeRegistration(..)
  , getStakeRegoKey
  , setStakeAddressRegistrationSlot
  -- ** Graph
  , Graph(..)
  , contributionAmount
  , getRegistrations
  , setRegistrations
  , modifyRegistrations
  , getGraphVote
  -- ** Helpers
  , UInteger(..)
  , SlotNo
  ) where

import           Cardano.API.Extended (VotingKeyPublic)
import           Cardano.Catalyst.Crypto (StakeSigningKey, getStakeVerificationKey,
                   stakeAddressFromVerificationKey)
import           Cardano.Catalyst.Registration (RewardsAddress, Vote, VotePayload (..),
                   catalystPurpose, createVoteRegistration, voteRegistrationSlot)
import           Cardano.Db.Extended ()
import           Data.Kind (Type)
import           Data.Maybe (catMaybes)
import           Data.Word (Word32)
import           Database.Persist.Postgresql (Key)
import           Database.Persist.Sql (Entity (..), entityKey, entityVal)

import qualified Cardano.Api as Api
import qualified Cardano.Db as Db

-- | The backbone of this module, 'PersistState' indicates whether the object:
--
--   - Is written to the database: 'Persisted'
--   - Is not written to the database: 'Ephemeral'
--
-- It is used as a type-level tag with 'DataKinds'.
data PersistState = Ephemeral | Persisted

-- | A transaction consists of:
--
--   - a 'Db.Tx',
--   - the 'Db.Block' it was accepted into the chain, and
--   - the 'Db.SlotLeader' for that 'Db.Block'.
--
-- This type is used by other types in this module that require the notion of a
-- "transaction".
class TransactionState (a :: PersistState) where
  data Transaction a :: Type
  transactionTx :: Transaction a -> Db.Tx
  -- ^ The database 'Db.Tx'.
  transactionBlock :: Transaction a -> Db.Block
  -- ^ The 'Db.Block' the 'Db.Tx' was accepted into the chain.
  transactionSlotLeader :: Transaction a -> Db.SlotLeader
  -- ^ The 'Db.SlotLeader' of that 'Db.Block'.

instance TransactionState 'Persisted where
  data Transaction 'Persisted =
    TransactionP { transactionTxP         :: Entity Db.Tx
                 , transactionBlockP      :: Entity Db.Block
                 , transactionSlotLeaderP :: Entity Db.SlotLeader
                 }
  transactionTx = entityVal . transactionTxP
  transactionBlock = entityVal . transactionBlockP
  transactionSlotLeader = entityVal . transactionSlotLeaderP

instance TransactionState 'Ephemeral where
  data Transaction 'Ephemeral =
    TransactionE { transactionTxE         :: Db.Tx
                 , transactionBlockE      :: Db.Block
                 , transactionSlotLeaderE :: Db.SlotLeader
                 }
  transactionTx = transactionTxE
  transactionBlock = transactionBlockE
  transactionSlotLeader = transactionSlotLeaderE

deriving instance Show (Transaction 'Ephemeral)
deriving instance Show (Transaction 'Persisted)
deriving instance Eq (Transaction 'Ephemeral)
deriving instance Eq (Transaction 'Persisted)
deriving instance Ord (Transaction 'Ephemeral)
deriving instance Ord (Transaction 'Persisted)

-- | Set the slot the transaction was accepted into the chain.
--
-- Available to 'Transaction's in the 'Ephemeral' state.
setTransactionSlot
  :: Maybe SlotNo
  -> Transaction 'Ephemeral
  -> Transaction 'Ephemeral
setTransactionSlot slotNo tx =
  let
    block = transactionBlockE tx
  in
    tx { transactionBlockE =
           block { Db.blockSlotNo = fromIntegral <$> slotNo }
       }

-- | Get the slot the transaction was accepted into the chain.
--
-- Available to 'Transaction's in /any/ state.
getTransactionSlot
  :: TransactionState state
  => Transaction state
  -> Maybe SlotNo
getTransactionSlot = fmap fromIntegral . Db.blockSlotNo . transactionBlock

-- | Get the ID of the transaction in the database.
--
-- Available to 'Transaction's in the 'Persisted' state.
getTxKey :: Transaction 'Persisted -> Key Db.Tx
getTxKey = entityKey . transactionTxP

-- | Get the ID of the block in the database.
--
-- Available to 'Transaction's in the 'Persisted' state.
getBlockKey :: Transaction 'Persisted -> Key Db.Block
getBlockKey = entityKey . transactionBlockP

-- | Get the ID of the slot leader in the database.
--
-- Available to 'Transaction's in the 'Persisted' state.
getSlotLeaderKey :: Transaction 'Persisted -> Key Db.SlotLeader
getSlotLeaderKey = entityKey . transactionSlotLeaderP

-- | An unspent transaction output consists of:
--
--    - the 'Transaction' the output came from, and
--    - the 'Db.TxOut' containing the details of that output.
--
-- To have any voting power, the stake address associated with a registration
-- must have at least one UTxO associated with it.
class UTxOState (a :: PersistState) where
  data UTxO a :: Type
  utxoTxOut :: UTxO a -> Db.TxOut
  -- ^ The unspent transaction out.
  utxoTx    :: UTxO a -> Transaction a
  -- ^ The transaction the UTxO came from.

instance UTxOState 'Persisted where
  data UTxO 'Persisted =
    UTxOP { utxoTxOutP :: Entity Db.TxOut
          , utxoTxP    :: Transaction 'Persisted
          }
  utxoTxOut = entityVal . utxoTxOutP
  utxoTx    = utxoTxP

instance UTxOState 'Ephemeral where
  data UTxO 'Ephemeral =
    UTxOE { utxoTxOutE :: Db.TxOut
          , utxoTxE :: Transaction 'Ephemeral
          }
  utxoTxOut = utxoTxOutE
  utxoTx    = utxoTxE

deriving instance Show (UTxO 'Ephemeral)
deriving instance Show (UTxO 'Persisted)
deriving instance Eq (UTxO 'Ephemeral)
deriving instance Eq (UTxO 'Persisted)
deriving instance Ord (UTxO 'Ephemeral)
deriving instance Ord (UTxO 'Persisted)

-- | Set the slot of the transaction the UTxO derives from.
--
-- Available to 'UTxO's in the 'Ephemeral' state.
setUTxOSlot
  :: SlotNo
  -> UTxO 'Ephemeral
  -> UTxO 'Ephemeral
setUTxOSlot slotNo utxo =
  let
    tx' = setTransactionSlot (Just slotNo) $ utxoTx utxo
  in
    utxo { utxoTxE = tx' }

-- | Get the unspent ADA value of the UTxO.
--
-- Available to 'UTxO's in the /any/ state.
utxoValue
  :: UTxOState state
  => UTxO state
  -> Integer
utxoValue = fromIntegral . Db.unDbLovelace . Db.txOutValue . utxoTxOut

-- | Set the stake address the UTxO is associated with.
--
-- Refers to the 'Db.StakeAddress' table.
--
-- Available to 'UTxO's in the 'Ephemeral' state.
setStakeAddressId
  :: Maybe (Key Db.StakeAddress)
  -> UTxO 'Ephemeral
  -> UTxO 'Ephemeral
setStakeAddressId stakeAddressId utxo =
  let
    txOut' = (utxoTxOut utxo) { Db.txOutStakeAddressId = stakeAddressId }
  in
    utxo { utxoTxOutE = txOut'
         , utxoTxE    = utxoTx utxo
         }

-- | Get the stake address ID of the stake address the UTxO.
--
-- Available to 'UTxO's in the /any/ state.
getStakeAddressId
  :: UTxOState state
  => UTxO state
  -> Maybe (Key Db.StakeAddress)
getStakeAddressId = Db.txOutStakeAddressId . utxoTxOut

-- | A vote registration is a 'Transaction' containing some relevant transaction
-- metadata (see 'Cardano.CLI.Voting.Metadata.voteToTxMetadata').
data Registration (state :: PersistState) = Registration
  { registrationVotePub        :: VotingKeyPublic
  -- ^ The side-chain public key voting power should be delegated to.
  , registrationRewardsAddress :: RewardsAddress
  -- ^ The main-chain address voter rewards should be sent to.
  , registrationSlotNo         :: SlotNo
  -- ^ The nonce used in the registration transaction (usually a slot number).
  , registrationSigningKey     :: StakeSigningKey
  -- ^ The stake signing key used to sign the transaction, and from which a
  -- stake address is derived.
  --
  -- The stake address should hold the funds that are delegated to voting.
  , registrationSigned         :: Bool
  -- ^ Whether or not the registration is signed.
  , registrationTx             :: Transaction state
  -- ^ The transaction in which the registration occurred, or will occur.
  }

deriving instance Show (Registration 'Ephemeral)
deriving instance Show (Registration 'Persisted)

-- | Get a vote registration from the transaction metadata of a 'Registration'.
--
-- May fail with Nothing if the transaction is unsigned.
--
-- Available to 'Registration's in /any/ state.
getRegistrationVote
  :: Registration state
  -> Maybe Vote
getRegistrationVote rego =
  if not (registrationSigned rego)
  then Nothing
  else
    Just $
        createVoteRegistration
          (registrationSigningKey rego)
          (registrationVotePub rego)
          (registrationRewardsAddress rego)
          (fromIntegral $ registrationSlotNo rego)

-- | Get the unsigned part of a vote registration from the 'Registration'.
--
-- Available to 'Registration's in /any/ state.
getRegistrationVotePayload
  :: Registration state
  -> VotePayload
getRegistrationVotePayload rego =
  VotePayload
    (registrationVotePub rego)
    (getStakeVerificationKey $ registrationSigningKey rego)
    (registrationRewardsAddress rego)
    (fromIntegral $ registrationSlotNo rego)
    (Just catalystPurpose)

-- | Indicate that the 'Registration' should be signed.
--
-- Available to 'Registration's in the 'Ephemeral' state.
signed :: Registration 'Ephemeral -> Registration 'Ephemeral
signed rego = rego { registrationSigned = True }

-- | Indicate that the 'Registration' should be unsigned. Unsigned registrations
-- are not considered when calculating voting power.
--
-- Available to 'Registration's in the 'Ephemeral' state.
unsigned :: Registration 'Ephemeral -> Registration 'Ephemeral
unsigned rego = rego { registrationSigned = False }

-- | Set the slot number of the transaction in which the vote registration was
-- made.
--
-- Available to 'Registration's in the 'Ephemeral' state.
setSlotNo
  :: SlotNo
  -> Registration 'Ephemeral
  -> Registration 'Ephemeral
setSlotNo slotNo rego =
  let
    tx' = setTransactionSlot (Just slotNo) $ registrationTx rego
  in
    rego { registrationSlotNo = slotNo
         , registrationTx = tx'
         }

-- | Stake addresses must be registered to the chain to be useful (see
-- https://developers.cardano.org/docs/stake-pool-course/handbook/register-stake-keys/
-- )
--
-- This type represents the stake address registration.
class StakeRegistrationState (state :: PersistState) where
  data StakeRegistration state :: Type
  -- | The stake signing key from which the stake address is derived.
  stakeRegoKey :: StakeRegistration state -> StakeSigningKey
  -- | The transaction which registered the stake address.
  stakeRegoTx  :: StakeRegistration state -> Transaction state
  -- | The stake address.
  stakeRegoAddress :: StakeRegistration state -> Db.StakeAddress

instance StakeRegistrationState 'Ephemeral where
  data StakeRegistration 'Ephemeral =
    StakeRegistrationE { stakeRegoKeyE  :: StakeSigningKey
                       , stakeRegoTxE   :: Transaction 'Ephemeral
                       , stakeRegoAddrE :: Db.StakeAddress
                       }
  stakeRegoKey = stakeRegoKeyE
  stakeRegoTx = stakeRegoTxE
  stakeRegoAddress = stakeRegoAddrE

instance StakeRegistrationState 'Persisted where
  data StakeRegistration 'Persisted =
    StakeRegistrationP { stakeRegoKeyP  :: StakeSigningKey
                       , stakeRegoTxP   :: Transaction 'Persisted
                       , stakeRegoAddrP :: Entity Db.StakeAddress
                       }
  stakeRegoKey     = stakeRegoKeyP
  stakeRegoTx      = stakeRegoTxP
  stakeRegoAddress = entityVal . stakeRegoAddrP

-- | Get the database ID of the 'Db.StakeAddress'.
--
-- Available to 'StakeRegistration's in the 'Persisted' state.
getStakeRegoKey :: StakeRegistration 'Persisted -> Key (Db.StakeAddress)
getStakeRegoKey = entityKey . stakeRegoAddrP

getStakeAddress
  :: StakeRegistrationState state
  => Api.NetworkId
  -> StakeRegistration state
  -> Api.StakeAddress
getStakeAddress nw =
  stakeAddressFromVerificationKey nw
  . getStakeVerificationKey
  . stakeRegoKey

deriving instance Show (StakeRegistration 'Ephemeral)
deriving instance Show (StakeRegistration 'Persisted)
deriving instance Eq (StakeRegistration 'Ephemeral)
deriving instance Eq (StakeRegistration 'Persisted)
deriving instance Ord (StakeRegistration 'Ephemeral)
deriving instance Ord (StakeRegistration 'Persisted)

-- | Set the slot the stake address was registered in.
--
-- Available to 'StakeRegistration's in the 'Ephemeral' state.
setStakeAddressRegistrationSlot
  :: SlotNo
  -> StakeRegistration 'Ephemeral
  -> StakeRegistration 'Ephemeral
setStakeAddressRegistrationSlot slotNo stakeRego =
  let
    tx' = setTransactionSlot (Just slotNo) $ stakeRegoTx stakeRego
  in
    stakeRego { stakeRegoTxE = tx' }

-- | A 'Graph' captures the relationship between:
--
--   - a 'StakeRegistration',
--   - the vote 'Registration's against that 'Db.StakeAddress', and
--   - the 'UTxO's staked to that 'Db.StakeAddress'.
--
-- It reflects the model used by voting-tools, where each stake address may have
-- zero or many registrations against it, and whose voting power is equal to the
-- sum of UTxOs associated with that stake address.
--
-- Each stake address may have many registrations against it, but only the
-- "latest" registration is valid.
--
-- You are not required to use this type, it is simply a convenient helper.
data Graph (state :: PersistState) = Graph
  { graphStakeAddressRegistration
      :: StakeRegistration (state :: PersistState)
  -- ^ The stake address.
  , graphRegistrations
      :: [Registration state]
  -- ^ Registrations for that stake address.
  , graphUTxOs
      :: [UTxO state]
  -- ^ The set of UTxOs associated with that stake address.
  }

deriving instance Show (Graph 'Ephemeral)
deriving instance Show (Graph 'Persisted)

-- | Get the total amount of ADA associated with the given 'Graph'.
--
-- Available to 'Graph's in /any/ state.
contributionAmount :: UTxOState state => Graph state -> Integer
contributionAmount (Graph _stkRego _regos utxos) =
  sum $ utxoValue <$> utxos

-- | Get the registrations associated with the given 'Graph'.
--
-- Available to 'Graph's in /any/ state.
getRegistrations :: Graph state -> [Registration state]
getRegistrations = graphRegistrations

-- | Set the registrations associated with the given 'Graph'.
--
-- Available to 'Graph's in the 'Ephemeral' state.
setRegistrations
  :: [Registration 'Ephemeral]
  -> Graph 'Ephemeral
  -> Graph 'Ephemeral
setRegistrations regos graph =
  graph { graphRegistrations = regos }

-- | Modify the registrations associated with the given 'Graph'.
--
-- Available to 'Graph's in the 'Ephemeral' state.
modifyRegistrations
  :: ([Registration 'Ephemeral] -> [Registration 'Ephemeral])
  -> Graph 'Ephemeral
  -> Graph 'Ephemeral
modifyRegistrations fn graph =
  graph { graphRegistrations = fn $ graphRegistrations graph }

-- | Attempt to parse a valid vote registration from the list of 'Registration's
-- associated with a 'Graph'.
--
-- Will choose the latest valid registration.
--
-- Available to 'Graph's in /any/ state.
getGraphVote
  :: Graph state
  -> Maybe Vote
getGraphVote (Graph _stkRego regos _utxos) =
  case Prelude.filter registrationSigned regos of
    [] ->
      Nothing
    signedRegos ->
      let
        indexedRegos =
          Prelude.zip [(1 :: Integer)..]
          $ catMaybes
          $ fmap getRegistrationVote signedRegos
      in
        case indexedRegos of
          [] -> Nothing
          x:xs ->
            Just $ snd $ Prelude.foldr chooseNewer x xs

chooseNewer
  :: Ord a
  => (a, Vote) -> (a, Vote) -> (a, Vote)
chooseNewer a b = if b `isNewer` a then b else a

-- | A newer registration will apply over an older one iff the nonce of the new
-- registration is greater than the old.
isNewer
  :: Ord a
  => (a, Vote)
  -> (a, Vote)
  -> Bool
isNewer a@(tA, _regoA) b@(tB, _regoB) =
  let
    (new, old) = if tA > tB then (a, b) else (b, a)

    slotNew = voteRegistrationSlot $ snd new
    slotOld = voteRegistrationSlot $ snd old
  in
    a == (if slotNew > slotOld then new else old)

-- | Positive integer in the range [0, 2147483647 ((2 ^ 31) - 1)].
--
-- 1. cardano-db-sync defines a "uinteger" type as an "integer" with a value >= 0.
-- 2. The Postgres "integer" type has a range [-2147483648, +2147483647].
-- 3. The range of a "uinteger" is therefore [0, +2147483647].
--
-- That is, we are restricting a 32-bit int to it's positive range.
--
-- The 'UInteger' type represents this cardano-db-sync "uinteger" type with
-- appropriate bounds.
--
-- It is useful when working with types that are written to the database using
-- the "uinteger" type (e.g. slot numbers).
--
-- NOTE: The cardano-db-sync API uses 'Data.Word.Word64' for slot numbers (eg.
-- 'Cardano.Db.blockSlotNo'), however this type will fail to write to the
-- database if it exceeds the range of a "uinteger".
newtype UInteger = UInteger Word32
  deriving (Eq, Enum, Ord, Real, Integral)
  deriving Show via Word32

instance Bounded UInteger where
  minBound = UInteger 0
  maxBound = UInteger $ (2 ^ (31 :: Int)) - 1

instance Num UInteger where
  (UInteger a) + (UInteger b) =
    fromInteger $ toInteger $ a + b

  (UInteger a) - (UInteger b) = UInteger $ a - b

  (UInteger a) * (UInteger b) =
    fromInteger $ toInteger $ a * b

  fromInteger a =
    if a > fromIntegral (maxBound :: UInteger)
    then 0
    else UInteger (fromInteger a)

  signum (UInteger i) = UInteger $ signum i

  abs (UInteger i) = UInteger $ abs i

type SlotNo = UInteger
