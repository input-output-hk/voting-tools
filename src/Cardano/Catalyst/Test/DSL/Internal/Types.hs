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

{- | WARNING: This is an internal module, it's interface is not safe. It is
recommended to import "Cardano.Catalyst.Test.DSL" instead. If you must import
this module, notify the maintainer of your use case so we can safely support it.

Module – Cardano.Catalyst.Test.DSL.Internal.Types
Description – Types for the testing DSL.
Maintainer – sevanspowell
Stability – experimental

This module provides the types necessary to construct terms in the "Test DSL".
It is recommended to use this DSL in conjunction with the
"Cardano.Catalyst.Test.Gen" module to generate appropriate terms.

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

Data types in this DSL follow the pattern of defining two variations of a type,
an 'Ephemeral' version (for data not committed to a database), and a
'Persistent' version (for data written to a database). The two variations have a
common interface (for data common to both types), and a specific interface (for
data only available to one of the two variants). This type-level encoding
provides safety when manipulating data that may or may not have been committed
to a database.

Terms can be converted from the 'Ephemeral' to 'Persistent' by using the
"Cardano.Catalyst.Test.DSL.Internal.Db" module, which includes code for
persisting the data to a database.
-}

module Cardano.Catalyst.Test.DSL.Internal.Types where

import           Cardano.API.Extended (VotingKeyPublic)
import           Cardano.CLI.Voting (createVoteRegistration)
import           Cardano.CLI.Voting.Metadata (RewardsAddress, Vote, VotePayload (..),
                   voteRegistrationSlot)
import           Cardano.CLI.Voting.Signing (VoteSigningKey, getVoteVerificationKey)
import           Cardano.Db.Extended ()
import           Data.Kind (Type)
import           Data.Maybe (catMaybes)
import           Data.Word (Word32)
import           Database.Persist.Postgresql (Key)
import           Database.Persist.Sql (Entity (..), entityKey, entityVal)

import qualified Cardano.Db as Db

-- | PersistState indicates whether the object:
--   - Is written to the database (Persisted)
--   - Is not written to the database (Ephemeral)
data PersistState = Ephemeral | Persisted

-- | Positive integer in the range [0, 2147483647 ((2 ^ 31) - 1)].
--
-- cardano-db-sync defines a "uinteger" type as an "integer" with a value >= 0.
-- The Postgres "integer" type has a range [-2147483648, +2147483647], so we're
-- restricting a 32-bit int to it's positive range.
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

-- TODO EXPOSE EPHEMERAL CONSTR NOT PERSISTED CONSTR

-- | A transaction consists of a Tx and the Block it was accepted into the
-- chain, also the SlotLeader for that Block.
class TransactionState (a :: PersistState) where
  data Transaction a :: Type
  transactionTx :: Transaction a -> Db.Tx
  -- ^ The database transaction.
  transactionBlock :: Transaction a -> Db.Block
  -- ^ The block the transaction was accepted into the chain.
  transactionSlotLeader :: Transaction a -> Db.SlotLeader
  -- ^ The slot leader of that block.

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
getTransactionSlot
  :: TransactionState state
  => Transaction state
  -> Maybe SlotNo
getTransactionSlot = fmap fromIntegral . Db.blockSlotNo . transactionBlock

-- | Get the ID of the transaction in the database.
getTxKey :: Transaction 'Persisted -> Key Db.Tx
getTxKey = entityKey . transactionTxP

-- | Get the ID of the block in the database.
getBlockKey :: Transaction 'Persisted -> Key Db.Block
getBlockKey = entityKey . transactionBlockP

-- | Get the ID of the slot leader in the database.
getSlotLeaderKey :: Transaction 'Persisted -> Key Db.SlotLeader
getSlotLeaderKey = entityKey . transactionSlotLeaderP

-- | A unspent transaction output consists of the transaction the output came
-- from and the value of that output. A UTxO contributes some value towards a
-- stake address when calculating voting power.
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
utxoValue
  :: UTxOState state
  => UTxO state
  -> Integer
utxoValue = fromIntegral . Db.unDbLovelace . Db.txOutValue . utxoTxOut

-- | Set the stake address id of the UTxO. Refers to the The StakeAddress table
-- index for the stake address part of the Shelley address.
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

-- | Get the stake address id of the UTxO. Refers to the The StakeAddress table
-- index for the stake address part of the Shelley address.
getStakeAddressId
  :: UTxOState state
  => UTxO state
  -> Maybe (Key Db.StakeAddress)
getStakeAddressId = Db.txOutStakeAddressId . utxoTxOut

-- | A vote registration is a transaction containing some relevant transaction
-- metadata (see 'voteToTxMetadata').
data Registration (state :: PersistState) = Registration
  { registrationVotePub        :: VotingKeyPublic
  , registrationRewardsAddress :: RewardsAddress
  , registrationSlotNo         :: SlotNo
  , registrationSigningKey     :: VoteSigningKey
  , registrationSigned         :: Bool
  , registrationTx             :: Transaction state
  }

deriving instance Show (Registration 'Ephemeral)
deriving instance Show (Registration 'Persisted)

-- | Get a vote registration from the transaction metadata of a 'Registration'.
--
-- May fail with Nothing if the transaction is unsigned.
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
getRegistrationVotePayload
  :: Registration state
  -> VotePayload
getRegistrationVotePayload rego =
  VotePayload
    (registrationVotePub rego)
    (getVoteVerificationKey $ registrationSigningKey rego)
    (registrationRewardsAddress rego)
    (fromIntegral $ registrationSlotNo rego)

-- | Indicate that the 'Registration' should be signed.
signed :: Registration 'Ephemeral -> Registration 'Ephemeral
signed rego = rego { registrationSigned = True }

-- | Indicate that the 'Registration' should be unsigned. Unsigned registrations
-- are not considered when calculating voting power.
unsigned :: Registration 'Ephemeral -> Registration 'Ephemeral
unsigned rego = rego { registrationSigned = False }

-- | Set the slot number of the transaction in which the vote registration was
-- made.
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
-- https://developers.cardano.org/docs/stake-pool-course/handbook/register-stake-keys/).
-- This type represents the stake address registration.
class StakeRegistrationState (state :: PersistState) where
  data StakeRegistration state :: Type
  -- | The stake signing key from which the stake address is derived.
  stakeRegoKey :: StakeRegistration state -> VoteSigningKey
  -- | The transaction which registered the stake address.
  stakeRegoTx  :: StakeRegistration state -> Transaction state
  -- | The stake address.
  stakeRegoAddress :: StakeRegistration state -> Db.StakeAddress

instance StakeRegistrationState 'Ephemeral where
  data StakeRegistration 'Ephemeral =
    StakeRegistrationE { stakeRegoKeyE  :: VoteSigningKey
                       , stakeRegoTxE   :: Transaction 'Ephemeral
                       , stakeRegoAddrE :: Db.StakeAddress
                       }
  stakeRegoKey = stakeRegoKeyE
  stakeRegoTx = stakeRegoTxE
  stakeRegoAddress = stakeRegoAddrE

instance StakeRegistrationState 'Persisted where
  data StakeRegistration 'Persisted =
    StakeRegistrationP { stakeRegoKeyP  :: VoteSigningKey
                       , stakeRegoTxP   :: Transaction 'Persisted
                       , stakeRegoAddrP :: Entity Db.StakeAddress
                       }
  stakeRegoKey     = stakeRegoKeyP
  stakeRegoTx      = stakeRegoTxP
  stakeRegoAddress = entityVal . stakeRegoAddrP

-- | Get the database id of the stake address.
getStakeRegoKey :: StakeRegistration 'Persisted -> Key (Db.StakeAddress)
getStakeRegoKey = entityKey . stakeRegoAddrP

deriving instance Show (StakeRegistration 'Ephemeral)
deriving instance Show (StakeRegistration 'Persisted)
deriving instance Eq (StakeRegistration 'Ephemeral)
deriving instance Eq (StakeRegistration 'Persisted)
deriving instance Ord (StakeRegistration 'Ephemeral)
deriving instance Ord (StakeRegistration 'Persisted)

-- | Set the slot the stake address was registered in.
setStakeAddressRegistrationSlot
  :: SlotNo
  -> StakeRegistration 'Ephemeral
  -> StakeRegistration 'Ephemeral
setStakeAddressRegistrationSlot slotNo stakeRego =
  let
    tx' = setTransactionSlot (Just slotNo) $ stakeRegoTx stakeRego
  in
    stakeRego { stakeRegoTxE = tx' }

-- | A 'Graph' captures the relationship between a 'StakeAddressRegistration',
-- the vote 'Registration's against that StakeAddress, and the 'UTxO's staked to
-- that StakeAddress.
--
-- It reflects the model used by voting-tools, where each stake address may have
-- zero or many registrations against it, and whose voting power is equal to the
-- sum of contributions made against it.
data Graph (state :: PersistState) = Graph
  { graphStakeAddressRegistration
      :: StakeRegistration (state :: PersistState)
  , graphRegistrations
      :: [Registration state]
  , graphUTxOs
      :: [UTxO state]
  }

deriving instance Show (Graph 'Ephemeral)
deriving instance Show (Graph 'Persisted)

-- | Get the total amount of ADA associated with the given 'Graph'.
contributionAmount :: UTxOState state => Graph state -> Integer
contributionAmount (Graph _stkRego _regos utxos) =
  sum $ utxoValue <$> utxos

-- | Get the registrations associated with the given 'Graph'.
getRegistrations :: Graph state -> [Registration state]
getRegistrations = graphRegistrations

-- | Set the registrations associated with the given 'Graph'.
setRegistrations
  :: [Registration 'Ephemeral]
  -> Graph 'Ephemeral
  -> Graph 'Ephemeral
setRegistrations regos graph =
  graph { graphRegistrations = regos }

-- | Modify the registrations associated with the given 'Graph'.
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
