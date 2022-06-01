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

Module      : Cardano.Catalyst.Test.DSL.Internal.Db
Description : Functions for writing Test DSL types to the database.
Maintainer  : sevanspowell
Stability   : experimental


__WARNING This is an internal module. It is recommended to import "Cardano.Catalyst.Test.DSL" instead.__

This module provides the code necessary to write the terms generated in
"Cardano.Catalyst.Test.DSL.Internal.Types" to the database.

In perticular it is capable of converting DSL terms from the
'Cardano.Catalyst.Test.DSL.Internal.Types.Ephemeral' to state
'Cardano.Catalyst.Test.DSL.Internal.Types.Persistent' state.

For example:

@
persistStakeRegistration
  :: SqlBackend
  -> 'StakeRegistration' \''Ephemeral'
  -> ReaderT SqlBackend IO ('StakeRegistration' \''Persisted')
persistStakeRegistration backend stakeRego =
  runSqlConn ('writeStakeRego' stakeRego) backend
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

-- | Query to write the 'Transaction' to the database.
--
-- Re-writes the foreign keys so that:
--
--   - The 'Cardano.Db.blockSlotLeaderId' points to the 'transactionSlotLeader'.
--   - The 'Cardano.Db.txBlockId' points to the 'transactionBlock'.
--
-- Returns the persisted 'Transaction'.
writeTx
  :: MonadIO m
  => Transaction 'Ephemeral
  -> ReaderT SqlBackend m (Transaction 'Persisted)
writeTx tx = do
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

-- | Query to write a 'UTxO' to the database.
--
-- Re-writes the foreign keys so that:
--
--   - The 'utxoTx' foreign keys are valid (see 'writeTx').
--   - The 'Cardano.Db.txOutTxId' comes from the 'utxoTx'.
--   - The 'Cardano.Db.txOutStakeAddressId' points to the given
--     'Cardano.Db.StakeAddress'.
--
-- Returns the persisted 'UTxO'.
writeUTxO
  :: (m ~ ReaderT SqlBackend IO)
  => Key Db.StakeAddress
  -> UTxO 'Ephemeral
  -> m (UTxO 'Persisted)
writeUTxO stakeAddressId utxo = do
  transaction' <- writeTx $ utxoTx utxo

  let txOut' = (utxoTxOut utxo) { Db.txOutTxId = getTxKey transaction'
                                , Db.txOutStakeAddressId = Just stakeAddressId
                                }
  txOutId <- Sql.insert txOut'

  pure $ UTxOP
    { utxoTxOutP = Entity txOutId txOut'
    , utxoTxP = transaction'
    }

-- | Query to write a 'Registration' to the database.
--
-- The 'Registration' is re-written so that:
--
--   - The 'registrationTx' foreign keys are valid (see 'writeTx').
--   - The 'Registration's 'Cardano.TxMetadata' is associated with the
--     'registrationTx'.
--
-- Returns the persisted 'Registration'.
writeRegistration
  :: (m ~ ReaderT SqlBackend IO)
  => Registration 'Ephemeral
  -> m (Registration 'Persisted)
writeRegistration rego = do
  transaction' <- writeTx (registrationTx rego)

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

-- | Query to write a 'StakeRegistration' to the database.
--
-- Re-writes the foreign keys so that:
--
--   - The 'stakeRegoTx's foreign keys are valid (see 'writeTx').
--   - The 'Db.stakeAddressRegisteredTxId' was registered in the 'stakeRegoTx'.
--
-- Returns the persisted 'StakeRegistration'.
writeStakeRego
  :: (m ~ ReaderT SqlBackend IO)
  => StakeRegistration 'Ephemeral
  -> m (StakeRegistration 'Persisted)
writeStakeRego stakeRego = do
  -- Write transaction in which stake address was registered
  stakeRegoTx' <- writeTx (stakeRegoTx stakeRego)

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

-- | Query to write a 'Graph' to the database.
--
-- The 'Graph' is re-written so that:
--
--   - The 'graphStakeAddressRegistration' foreign keys are valid (see
--     'writeStakeRego').
--   - The 'graphRegistrations' foreign keys are valid (see
--     'writeRegistration').
--   - The 'graphUTxOs' foreign keys are valid (see 'writeUTxO').
--
-- Returns the persisted 'Graph'.
writeGraph
  :: (m ~ ReaderT SqlBackend IO)
  => Graph 'Ephemeral
  -> m (Graph 'Persisted)
writeGraph (Graph stakeRego regos utxos) = do
  -- Write stake address registration
  stakeRego' <- writeStakeRego stakeRego

  -- Write contributions against stake address
  utxos' <- traverse (writeUTxO $ getStakeRegoKey stakeRego') utxos

  -- Write registrations
  regos' <- traverse writeRegistration regos

  -- Return re-written Graph
  pure $ Graph stakeRego' regos' utxos'

-- | Convert between cardano-api tx metadata and cardano-db-sync tx metadata.
--
-- Converts 'Cardano.TxMetadata' to a list of 'Db.TxMetadata' entries.
apiToDbMetadata
  :: Cardano.TxMetadata
  -- ^ Cardano.Api.TxMetadata.
  -> Key Db.Tx
  -- ^ Database key of the transaction to which this transaction metadata should
  -- belong.
  -> [Db.TxMetadata]
  -- ^ Cardano.Db.TxMetadata ephemeral database entities (not yet written to
  -- database).
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
