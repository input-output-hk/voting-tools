{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Catalyst.Query.Sql where

import           Cardano.Catalyst.Query.Types (Query)
import           Cardano.Db (DbLovelace (DbLovelace), TxId)
import           Control.Monad.Except (runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Either (rights)
import           Data.Monoid (Sum (..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Traversable (forM)
import           Database.Persist.Postgresql (BackendCompatible, Single (..), SqlBackend,
                   rawExecute, rawSql)
import           Ouroboros.Network.Block (unSlotNo)

import           Cardano.Api (SlotNo)
import qualified Cardano.Api as Api
import qualified Cardano.Catalyst.Query.Types as Query
import           Cardano.Catalyst.Registration (MetadataParsingError, metadataMetaKey,
                   signatureMetaKey)
import           Control.Lens ((#))
import           Control.Lens.TH (makeClassyPrisms)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM

data MetadataRetrievalError
  = MetadataFailedToRetrieveMetadataField !TxId
  | MetadataFailedToRetrieveSignatureField !TxId
  | MetadataFailedToDecodeMetadataField !TxId !Text
  | MetadataFailedToDecodeSignatureField !TxId !Text
  | MetadataFailedToDecodeTxMetadata !TxId !Api.TxMetadataJsonError
  | MetadataFailedToParseVoteRegistration !TxId !MetadataParsingError
  deriving (Eq, Show)

makeClassyPrisms ''MetadataRetrievalError

sqlQuery
  :: MonadIO m
  => Query (ReaderT SqlBackend m) TxId
sqlQuery =
  Query.Query { Query.queryVoteRegistrations = queryVoteRegistrations
              , Query.queryStakeValue = queryStakeValue
              , Query.queryStakeValues = queryStakeValues
              }

queryVoteRegistrations
  :: ( MonadIO m
     , MonadReader backend m
     , BackendCompatible SqlBackend backend
     )
  => Maybe SlotNo
  -> m [(TxId, Aeson.Value)]
queryVoteRegistrations mSlotNo =
  let
    -- Join the transaction information with the metadata information for that
    -- transaction. The metadata we are interested in is made up of two parts:
    -- the metadata value under key '61284' (voting metadata) and the metadata
    -- under the key '61285' (voting signature metadata).
    sqlBase = "WITH meta_table AS (select tx_id, json AS metadata from tx_metadata where key = '" <> T.pack (show metadataMetaKey) <> "') , sig_table AS (select tx_id, json AS signature from tx_metadata where key = '" <> T.pack (show signatureMetaKey) <> "') SELECT tx.hash,tx_id,metadata,signature FROM meta_table INNER JOIN tx ON tx.id = meta_table.tx_id INNER JOIN sig_table USING(tx_id)"
  in do
    let
      sql = case mSlotNo of
        Just slot -> (sqlBase <> "INNER JOIN block ON block.id = tx.block_id WHERE block.slot_no <= " <> T.pack (show $ unSlotNo slot) <> " ORDER BY metadata -> '4' ASC;")
        -- ^ TODO handle lower bound on slot no too
        Nothing   -> (sqlBase <> " ORDER BY metadata -> '4' ASC;")
    r <- ask
    (results :: [(Single ByteString, Single TxId, Single (Maybe Text), Single (Maybe Text))]) <- (flip runReaderT) r $ rawSql sql []
    (parseResults :: [Either MetadataRetrievalError (TxId, Aeson.Value)]) <-
      sequence $ fmap runExceptT $ (flip fmap) results $ \(Single _txHash, Single txId, Single mMetadata, Single mSignature) -> do
        let
            handleEither f =
                either (throwError . f) pure
        -- DECISION #01:
        --   When querying the transaction/metadata/signature information, the
        --   given row did not have a metadata entry under the key '61284' (i.e.
        --   it did not have any voting metadata).
        --
        --   Found entry
        --   └── But it contained no voting metadata
        --
        -- FIXME: Isn't this prevented by the query? Wouldn't it always be Just?
        -- Answer is yes - there is no need to make this a "Maybe".
        metadata  <-
            maybe
            (throwError $ _MetadataFailedToRetrieveMetadataField # txId)
            pure
            mMetadata
        -- DECISION #02:
        --   When querying the transaction/metadata/signature information, the
        --   given row did not have a metadata signature entry under the key
        --   '61285' (i.e. it did not have any signature metadata).
        --
        --   Found entry
        --   └── But it contained no signature information
        --
        -- FIXME: Isn't this prevented by the query? Wouldn't it always be Just?
        -- Answer is yes - there is no need to make this a "Maybe".
        signature <-
            maybe
            (throwError $ _MetadataFailedToRetrieveSignatureField # txId)
            pure
            mSignature

        -- DECISION #03:
        --   We found an entry with the right keys but failed to parse the voting
        --   metadata because it wasn't a JSON value.
        --
        --   Found entry
        --   └── It had the right metadata keys
        --       └── But the voting metadata value wasn't JSON
        --
        -- This is programmer error - the database should only accept JSON values
        -- into the 'json' column, and even if it doesn't, the tool that submits
        -- the data should only submit valid json. This isn't the voter's fault.
        metadataObj <-
            handleEither (\e -> _MetadataFailedToDecodeMetadataField # (txId, T.pack e))
            $ Aeson.eitherDecode' $ TL.encodeUtf8 $ TL.fromStrict $ metadata
        -- DECISION #04:
        --   We found an entry with the right keys but failed to parse the signature
        --   metadata because it wasn't a JSON value.
        --
        --   Found entry
        --   └── It had the right metadata keys
        --       └── But the signature metadata value wasn't JSON
        --
        -- This is programmer error - the database should only accept JSON values
        -- into the 'json' column, and even if it doesn't, the tool that submits
        -- the data should only submit valid json. This isn't the voter's fault.
        signatureObj <-
            handleEither (\e -> _MetadataFailedToDecodeSignatureField # (txId, T.pack e))
            $ Aeson.eitherDecode' $ TL.encodeUtf8 $ TL.fromStrict $ signature

        let
          metaObj :: Aeson.Value
          metaObj = Aeson.Object $ HM.fromList
            [ (T.pack $ show metadataMetaKey, metadataObj)
            , (T.pack $ show signatureMetaKey, signatureObj)
            ]

        pure $ (txId, metaObj)
    pure $ rights $ parseResults

queryStakeValues
  :: MonadIO m
  => Maybe SlotNo
  -> [Api.StakeAddress]
  -> ReaderT SqlBackend m [(Api.StakeAddress, Integer)]
queryStakeValues mSlotNo stakeAddrs = do
  mkStakeSnapshotTable mSlotNo
  forM stakeAddrs $ \stakeAddr -> do
    stake <- queryStakeValue' mSlotNo stakeAddr
    pure (stakeAddr, stake)

queryStakeValue
  :: MonadIO m
  => Maybe Api.SlotNo
  -> Api.StakeAddress
  -> ReaderT SqlBackend m Integer
queryStakeValue mSlotNo stakeAddr = do
  mkStakeSnapshotTable mSlotNo
  queryStakeValue' mSlotNo stakeAddr

queryStakeValue'
  :: MonadIO m
  => Maybe Api.SlotNo
  -> Api.StakeAddress
  -> ReaderT SqlBackend m Integer
queryStakeValue' _mSlotNo stakeAddress = do
  let
      stakeAddressHex = T.pack (BC.unpack $ Api.serialiseToRawBytesHex stakeAddress)
      -- Don't do SUM here, lovelace is a bounded integer type defined by
      -- cardano-db-sync, unless you perform a conversion to an unbounded type,
      -- it will overflow if the SUM exceeds the max value of a lovelace db
      -- type.
      stakeQuerySql = "SELECT utxo_snapshot.value FROM utxo_snapshot WHERE stake_credential = decode('" <> stakeAddressHex <> "', 'hex');"
  (stakeValues :: [Single (Maybe DbLovelace)]) <- rawSql stakeQuerySql []
  pure $ getSum $ foldMap (\case
                           Single Nothing ->
                             Sum 0
                           Single (Just (DbLovelace stake)) ->
                             Sum $ fromIntegral stake
                       ) stakeValues

mkStakeSnapshotTable
  :: MonadIO m
  => Maybe Api.SlotNo
  -> ReaderT SqlBackend m ()
-- Voting power is calculated from unspent UTxOs, so we look for TxOuts that
-- have no associated TxIn.
-- In the following, txIn.tx_in_id is NULL when the transaction output has not
-- been spent (due to the left outer join).
mkStakeSnapshotTable Nothing = do
  let stake_credential_index = "CREATE INDEX IF NOT EXISTS utxo_snapshot_stake_credential ON utxo_snapshot(stake_credential);"
      analyze_table = "ANALYZE utxo_snapshot;"
      utxo_snapshot = "CREATE TEMPORARY TABLE IF NOT EXISTS utxo_snapshot AS (SELECT tx_out.*, stake_address.hash_raw AS stake_credential FROM tx_out LEFT OUTER JOIN tx_in ON tx_out.tx_id = tx_in.tx_out_id AND tx_out.index = tx_in.tx_out_index INNER JOIN stake_address ON stake_address.id = tx_out.stake_address_id WHERE tx_in.tx_in_id IS NULL);"
  rawExecute (utxo_snapshot <> stake_credential_index <> analyze_table) []
mkStakeSnapshotTable (Just slotNo) = do
  let tx_out_snapshot = "CREATE TEMPORARY TABLE IF NOT EXISTS tx_out_snapshot AS (\
        \ SELECT tx_out.*,\
        \ stake_address.hash_raw AS stake_credential\
          \ FROM tx_out\
          \ INNER JOIN tx ON tx_out.tx_id = tx.id\
          \ INNER JOIN block ON tx.block_id = block.id\
          \ INNER JOIN stake_address ON stake_address.id = tx_out.stake_address_id\
          \ WHERE block.slot_no <= " <> T.pack (show $ unSlotNo slotNo) <> ");"
      tx_in_snapshot = "CREATE TEMPORARY TABLE IF NOT EXISTS tx_in_snapshot AS (\
        \ SELECT tx_in.* FROM tx_in\
          \ INNER JOIN tx ON tx_in.tx_in_id = tx.id\
          \ INNER JOIN block ON tx.block_id = block.id\
          \ WHERE block.slot_no <= " <> T.pack (show $ unSlotNo slotNo) <> ");"
      utxo_snapshot = "CREATE TEMPORARY TABLE IF NOT EXISTS utxo_snapshot AS (\
        \ SELECT tx_out_snapshot.* FROM tx_out_snapshot\
          \ LEFT OUTER JOIN tx_in_snapshot\
            \ ON tx_out_snapshot.tx_id = tx_in_snapshot.tx_out_id\
            \ AND tx_out_snapshot.index = tx_in_snapshot.tx_out_index\
          \ WHERE tx_in_snapshot.tx_in_id IS NULL);"
      stake_credential_index = "CREATE INDEX IF NOT EXISTS utxo_snapshot_stake_credential ON utxo_snapshot(stake_credential);"
      analyze_tx_out_snapshot = "ANALYZE tx_out_snapshot;"
      analyze_tx_in_snapshot = "ANALYZE tx_in_snapshot;"
      analyze_utxo_snapshot = "ANALYZE utxo_snapshot;"
  rawExecute (  tx_out_snapshot
             <> analyze_tx_out_snapshot
             <> tx_in_snapshot
             <> analyze_tx_in_snapshot
             <> utxo_snapshot
             <> stake_credential_index
             <> analyze_utxo_snapshot
             ) []
