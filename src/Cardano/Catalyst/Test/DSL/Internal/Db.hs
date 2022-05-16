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

Module – Cardano.Catalyst.Test.DSL.Internal.Db
Description – Functions for writing Test DSL types to the database.
Maintainer – sevanspowell
Stability – experimental

This module provides the code necessary to write the terms generated in
"Cardano.Catalyst.Test.DSL.Internal.Types" to the database.

In perticular it is capable of converting DSL terms from the 'Ephemeral' to
'Persistent' state.

For example:

@
persistStakeRegistration
  :: 'StakeRegistration' \''Ephemeral'
  -> ReaderT SqlBackend IO ('StakeRegistration' \''Persisted')
persistStakeRegistration stakeRego =
  'stakeRegoToQuery' stakeRego
@
-}

module Cardano.Catalyst.Test.DSL.Internal.Db where

import           Cardano.CLI.Voting.Metadata (votePayloadToTxMetadata, voteToTxMetadata)
import           Cardano.Catalyst.Test.DSL.Internal.Types (Graph (..), PersistState (..),
                   Registration (..), StakeRegistration (..), Transaction (..), UTxO (..),
                   getRegistrationVote, getRegistrationVotePayload, getStakeRegoKey, getTxKey,
                   stakeRegoAddress, stakeRegoKey, stakeRegoTx, transactionBlock,
                   transactionSlotLeader, transactionTx, utxoTx, utxoTxOut)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (ReaderT)
import           Data.Foldable (traverse_)
import           Data.Function ((&))
import           Data.Word (Word64)
import           Database.Persist.Postgresql (Key)
import           Database.Persist.Sql (Entity (..), SqlBackend)

import qualified Cardano.Api as Cardano
import qualified Cardano.Db as Db
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.Text.Encoding as T
import qualified Database.Persist.Class as Sql

-- | Query to write the @Transaction@ to the database. Re-writes the foreign
-- keys so that:
--   - The block points to the slot leader.
--   - The tx points to the block.
--
-- Returns the persisted Transaction.
txToQuery
  :: MonadIO m
  => Transaction 'Ephemeral
  -> ReaderT SqlBackend m (Transaction 'Persisted)
txToQuery tx = do
  let slotLeader = transactionSlotLeader tx
  slotLeaderId <- Sql.insert slotLeader

  let block' = (transactionBlock tx) { Db.blockSlotLeaderId = slotLeaderId }
  blockId <- Sql.insert block'

  let tx' = (transactionTx tx) { Db.txBlockId = blockId }
  txId <- Sql.insert tx'

  pure $ TransactionP
    { transactionTxP = Entity txId tx'
    , transactionBlockP = Entity blockId block'
    , transactionSlotLeaderP = Entity slotLeaderId slotLeader
    }

-- | Query to write a 'UTxO' to the database. Re-writes the foreign keys so
-- that:
--   - The TxOut comes from the transaction.
--   - The TxOut points to the given stake address.
--
-- Returns the persisted 'UTxO'.
utxoToQuery
  :: (m ~ ReaderT SqlBackend IO)
  => Key Db.StakeAddress
  -> UTxO 'Ephemeral
  -> m (UTxO 'Persisted)
utxoToQuery stakeAddressId utxo = do
  transaction' <- txToQuery $ utxoTx utxo

  let txOut' = (utxoTxOut utxo) { Db.txOutTxId = getTxKey transaction'
                                , Db.txOutStakeAddressId = Just stakeAddressId
                                }
  txOutId <- Sql.insert txOut'

  pure $ UTxOP
    { utxoTxOutP = Entity txOutId txOut'
    , utxoTxP = transaction'
    }

-- | Query to write a 'Registration' to the database. The 'Registration' is
-- re-written so that:
--   - The TxMetadata points to the Tx.
--   - Transaction foreign keys are valid.
--
-- Returns the persisted 'Registration'.
registrationToQuery
  :: (m ~ ReaderT SqlBackend IO)
  => Registration 'Ephemeral
  -> m (Registration 'Persisted)
registrationToQuery rego = do
  transaction' <- txToQuery (registrationTx rego)

  let
    voteMeta :: Cardano.TxMetadata
    voteMeta =
      case getRegistrationVote rego of
        Nothing ->
          votePayloadToTxMetadata $ getRegistrationVotePayload rego
        Just vote ->
          voteToTxMetadata vote

  let
    txId = getTxKey transaction'

  traverse_ Sql.insert (apiToDbMetadata voteMeta txId )

  pure $ rego { registrationTx = transaction' }

-- Convert 'Cardano.TxMetadata' to a list of 'Db.TxMetadata' entries.
apiToDbMetadata
  :: Cardano.TxMetadata
  -- ^ Transaction metadata.
  -> Key Db.Tx
  -- ^ Database key of the transaction to which this transaction metadata should
  -- belong.
  -> [Db.TxMetadata]
  -- ^ Ephemeral database entities (not yet written to database).
apiToDbMetadata txMeta txId =
  let
    metaMap :: M.Map Word64 Cardano.TxMetadataValue
    (Cardano.TxMetadata metaMap) = txMeta

    metaMapJSON :: M.Map Word64 Aeson.Value
    metaMapJSON =
      fmap Cardano.metadataValueToJsonNoSchema metaMap

    dbMeta :: [Db.TxMetadata]
    dbMeta =
      M.toList metaMapJSON
      & fmap (\(k, json) ->
                let
                  jsonBytes = BSL.toStrict $ Aeson.encode json
                  jsonText = T.decodeUtf8 jsonBytes
                in
                  Db.TxMetadata
                    (Db.DbWord64 k)
                    (Just jsonText)
                    jsonBytes
                    txId
             )
  in
    dbMeta

-- | Query to write a 'StakeRegistration' to the database. Re-writes the foreign
-- keys so that:
--   - The StakeAddress was registered in the transaction.
--   - The TxOut points to the given stake address.
--
-- Returns the persisted 'StakeRegistration'.
stakeRegoToQuery
  :: (m ~ ReaderT SqlBackend IO)
  => StakeRegistration 'Ephemeral
  -> m (StakeRegistration 'Persisted)
stakeRegoToQuery stakeRego = do
  -- Write transaction in which stake address was registered
  stakeRegoTx' <- txToQuery (stakeRegoTx stakeRego)

  let stakeRegoTxId = getTxKey stakeRegoTx'

  -- Write stake address
  let
    stakeAddr' =
      (stakeRegoAddress stakeRego) {
        Db.stakeAddressRegisteredTxId = stakeRegoTxId
      }
  stakeAddrId <- Sql.insert stakeAddr'

  pure $
    StakeRegistrationP
      (stakeRegoKey stakeRego)
      stakeRegoTx'
      (Entity stakeAddrId stakeAddr')

-- | Query to write a 'Graph' to the database. The 'Graph' is re-written so
-- that:
--   - The stake registration Transaction foreign keys are valid (see
--     'txToQuery').
--   - The stake address is registered in the correct transaction.
--   - The UTxOs foreign keys are valid (see 'utxoToQuery').
--   - The Registrations foreign keys are valid (see 'registrationToQuery').
--
-- Returns the persisted Graph.
graphToQuery
  :: (m ~ ReaderT SqlBackend IO)
  => Graph 'Ephemeral
  -> m (Graph 'Persisted)
graphToQuery (Graph stakeRego regos utxos) = do
  -- Write stake address registration
  stakeRego' <- stakeRegoToQuery stakeRego

  -- Write contributions against stake address
  utxos' <- traverse (utxoToQuery $ getStakeRegoKey stakeRego') utxos

  -- Write registrations
  regos' <- traverse registrationToQuery regos

  -- Return re-written Graph
  pure $ Graph stakeRego' regos' utxos'
