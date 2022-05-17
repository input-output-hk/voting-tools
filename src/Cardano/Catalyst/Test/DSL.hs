{- |

Module      : Cardano.Catalyst.Test.DSL
Description : Testing DSL.
Maintainer  : sevanspowell
Stability   : experimental

Collection of types and functions used to form the testing DSL for voting-tools.

For further documentation and usage, see the documentation for:

  - "Cardano.Catalyst.Test.DSL.Internal.Types"
  - "Cardano.Catalyst.Test.DSL.Internal.Db"
  - "Cardano.Catalyst.Test.DSL.Gen"
-}

module Cardano.Catalyst.Test.DSL
  ( -- * Types
    Types.PersistState(..)
  , Types.UInteger(..)
  , Types.SlotNo
  -- ** Transaction
  , Types.Transaction(Types.TransactionE)
  , Types.transactionTx
  , Types.transactionBlock
  , Types.transactionSlotLeader
  , Types.setTransactionSlot
  , Types.getTransactionSlot
  , Types.getTxKey
  , Types.getBlockKey
  , Types.getSlotLeaderKey
  -- ** UTxO
  , Types.UTxO(Types.UTxOE)
  , Types.utxoTxOut
  , Types.utxoTx
  , Types.setUTxOSlot
  , Types.utxoValue
  , Types.setStakeAddressId
  , Types.getStakeAddressId
  -- ** Registration
  , Types.Registration(..)
  , Types.getRegistrationVote
  , Types.getRegistrationVotePayload
  , Types.signed
  , Types.unsigned
  , Types.setSlotNo
  -- ** Stake Registration
  , Types.StakeRegistration(Types.StakeRegistrationE)
  , Types.stakeRegoKey
  , Types.stakeRegoTx
  , Types.stakeRegoAddress
  , Types.getStakeRegoKey
  , Types.getStakeAddress
  , Types.setStakeAddressRegistrationSlot
  -- ** Graph
  , Types.Graph(..)
  , Types.contributionAmount
  , Types.getRegistrations
  , Types.setRegistrations
  , Types.modifyRegistrations
  , Types.getGraphVote

  -- * Database
  , Db.writeTx
  , Db.writeUTxO
  , Db.writeRegistration
  , Db.writeStakeRego
  , Db.writeGraph
  , Db.apiToDbMetadata

  -- * Generators
  , Gen.genUniqueHash32
  , Gen.genUniqueHash28
  , Gen.genUTCTime
  , Gen.genLovelace
  , Gen.genSlotLeader
  , Gen.genTxMetadata
  , Gen.genWord64
  , Gen.genWord32
  , Gen.genUInteger
  , Gen.genWord63
  , Gen.genWord16
  , Gen.genInt64
  , Gen.genBlock
  , Gen.genTx
  , Gen.genTransaction
  , Gen.genVoteRegistration
  , Gen.genStakeAddressRegistration
  , Gen.genStakeAddress
  , Gen.genStakeAddressForVerificationKey
  , Gen.genTxOut
  , Gen.genTxIn
  , Gen.genUTxO
  , Gen.genGraph
  ) where

import qualified Cardano.Catalyst.Test.DSL.Gen as Gen
import qualified Cardano.Catalyst.Test.DSL.Internal.Db as Db
import qualified Cardano.Catalyst.Test.DSL.Internal.Types as Types
