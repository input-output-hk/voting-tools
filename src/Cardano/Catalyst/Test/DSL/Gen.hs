{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.Catalyst.Test.DSL.Gen where

import           Cardano.CLI.Voting.Signing (VoteSigningKey, VoteVerificationKey,
                   getVoteVerificationKey, serialiseVoteVerificationKeyToBech32,
                   voteVerificationKeyStakeAddressHashRaw)
import           Cardano.Catalyst.Test.DSL.Internal.Types (Graph (..), PersistState (..),
                   Registration (..), StakeRegistration (..), Transaction (..), UTxO (..),
                   stakeRegoKey)
import           Control.Monad (replicateM_)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.State.Class (MonadState)
import           Data.ByteString (ByteString)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int16, Int64)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word16, Word32, Word64)
import           Hedgehog (GenBase, MonadGen, fromGenT)

import qualified Cardano.Api as Cardano
import qualified Cardano.Db as Db
import qualified Control.Monad.State.Class as State
import qualified Data.Aeson as Aeson
import qualified Data.Binary.Put as Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as T
import qualified Data.Time.Clock.POSIX as Time
import qualified Database.Persist.Sql as Persist
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Generators as Gen

cardanoGenTxMetadata :: MonadGen m => m Cardano.TxMetadata
cardanoGenTxMetadata =
    fmap (Cardano.TxMetadata . Map.fromList) $ do
        Gen.list (Range.linear 0 10)
            ((,) <$> Gen.word64 (Range.linearBounded)
                 <*> cardanoGenTxMetadataValue)

cardanoGenTxMetadataValue :: MonadGen m => m Cardano.TxMetadataValue
cardanoGenTxMetadataValue =
    Gen.recursive
      Gen.choice
      [ Cardano.TxMetaNumber <$> genTxMetaNumber
      , Cardano.TxMetaBytes <$> genTxMetaBytes
      , Cardano.TxMetaText <$> genTxMetaText
      ]
      [ Cardano.TxMetaList <$> genTxMetaList
      , Cardano.TxMetaMap <$> genTxMetaMap
      ]
    where
        genTxMetaNumber :: MonadGen m => m Integer
        genTxMetaNumber = fromIntegral <$> Gen.int64 Range.linearBounded

        genTxMetaBytes :: MonadGen m => m ByteString
        genTxMetaBytes = Gen.bytes (Range.linear 0 64)

        genTxMetaText :: MonadGen m => m Text
        genTxMetaText = Gen.text (Range.linear 0 64) Gen.alphaNum

        genTxMetaList :: MonadGen m => m [Cardano.TxMetadataValue]
        genTxMetaList = Gen.list (Range.linear 0 10) cardanoGenTxMetadataValue

        genTxMetaMap
          :: MonadGen m
          => m [(Cardano.TxMetadataValue, Cardano.TxMetadataValue)]
        genTxMetaMap =
            Gen.list (Range.linear 0 10) $
                (,) <$> cardanoGenTxMetadataValue <*> cardanoGenTxMetadataValue

genUniqueHash32
  :: ( MonadGen m
     , MonadState [Word32] m
     )
  => m ByteString
genUniqueHash32 =
  -- Discard shrink tree (i.e. don't shrink). If we shrink we risk duplicate
  -- hashes.
  -- Gen.prune $ do
  do
    -- Get a Word32 from our list
    xs <- State.get
    -- Make sure this Word32 is not used again
    State.put $ tail xs
    let
      -- A Word32 is 4 bytes, but we need a 32-byte hash. So replicate the 4
      -- bytes 8 times. This doesn't cover all possible 32-bytes, but it should
      -- be enough.
      bytes = Put.runPut $ do
        let word32 = head xs
        replicateM_ 8 $ Put.putWord32be $ word32
    pure $ BSL.toStrict bytes

genUniqueHash28
  :: ( MonadGen m
     , MonadState [Word32] m
     )
  => m ByteString
genUniqueHash28 = do
  hash32 <- genUniqueHash32
  -- Get the 28 most significant bytes of the 32-byte hash. Due to the way
  -- genUniqueHash32 is implemented, this won't result in duplicate hashes.
  pure $ BS.take 28 hash32

genUTCTime :: MonadGen m => m UTCTime
genUTCTime =
  (Time.posixSecondsToUTCTime . fromIntegral)
  <$> (Gen.word32 Range.linearBounded)

genHash32 :: MonadGen m => m ByteString
genHash32 = Gen.bytes (Range.singleton 32)

genHash28 :: MonadGen m => m ByteString
genHash28 = Gen.bytes (Range.singleton 28)

genLovelace :: MonadGen m => m Db.DbLovelace
genLovelace =
  Db.DbLovelace <$> Gen.integral (Range.linear 0 18446744073709551615)

genSlotLeader :: (MonadGen m, MonadState [Word32] m) => m Db.SlotLeader
genSlotLeader = Db.SlotLeader
  <$> genUniqueHash28
  -- ^ Addresses use a 28 byte hash (as do StakeholdIds).
  <*> pure Nothing
  <*> Gen.text (Range.linear 0 64) Gen.unicodeAll

genTxMetadata
  :: ( MonadGen m
     , GenBase m ~ Identity
     )
  => m Db.TxMetadata
genTxMetadata = do
  mMetadata <- Gen.maybe $ fromGenT cardanoGenTxMetadata

  let
    (jsonText, jsonBytes) = case mMetadata of
        Nothing ->
          (Nothing, "")
        Just meta ->
          let
            jsonValue =
              Cardano.metadataToJson Cardano.TxMetadataJsonDetailedSchema meta
            jsonBs = BSL.toStrict $ Aeson.encode jsonValue
            jsonTxt = T.decodeUtf8 jsonBs
          in
            (Just jsonTxt, jsonBytes)

  Db.TxMetadata
    <$> (Db.DbWord64 <$> genWord64)
    <*> pure jsonText
    <*> pure jsonBytes
    <*> (Persist.toSqlKey <$> genInt64)

genWord64 :: MonadGen m => m Word64
genWord64 = Gen.word64 Range.linearBounded

genWord32 :: MonadGen m => m Word32
genWord32 = Gen.word32 Range.linearBounded

-- | Positive integer in the range [0, 2147483647 ((2 ^ 31) - 1)].
--
-- cardano-db-sync defines a "uinteger" type as an "integer" with a value >= 0.
-- The Postgres "integer" type has a range [-2147483648, +2147483647], so we're
-- restricting a 32-bit int to it's positive range.
genUInteger :: (MonadGen m, Num int) => m int
genUInteger = fromIntegral <$> Gen.int32 (Range.linear 0 maxBound)

genWord63 :: MonadGen m => m Word64
genWord63 = Gen.word64 (Range.linear 0 (2 ^ (63 :: Word64)))

genWord16 :: MonadGen m => m Word16
genWord16 = Gen.word16 Range.linearBounded

genInt64 :: MonadGen m => m Int64
genInt64 = Gen.int64 Range.linearBounded

genBlock
  :: ( MonadGen m
     , MonadState [Word32] m
     )
  => m Db.Block
genBlock = Db.Block
  <$> genUniqueHash32
  -- ^ Hash
  <*> Gen.maybe genUInteger
  -- ^ epoch number
  <*> Gen.maybe genUInteger
  -- ^ slot number
  <*> Gen.maybe genUInteger
  -- ^ epoch slot no
  <*> Gen.maybe genUInteger
  -- ^ block no
  <*> pure Nothing
  -- ^ previous block id
  <*> (Persist.toSqlKey <$> genInt64)
  -- ^ slot leader id
  <*> genUInteger
  -- ^ block size
  <*> genUTCTime
  --- ^ block time
  <*> genWord64
  -- ^ Tx count
  <*> genWord16
  -- ^ Protocol major ver
  <*> genWord16
  -- ^ Protocol major ver
  <*> Gen.maybe (Gen.text (Range.singleton 65) Gen.ascii)
  -- ^ vrf key
  <*> Gen.maybe genHash32
  -- ^ op cert
  <*> Gen.maybe genWord63
  -- ^ op cert counter

genTx
  :: ( MonadGen m
     , MonadState [Word32] m
     )
  => m Db.Tx
genTx = Db.Tx
  <$> genUniqueHash32
  <*> (Persist.toSqlKey <$> genInt64)
  -- ^ Block id
  <*> genUInteger
  -- ^ Block index
  <*> genLovelace
  -- ^ out_sum
  <*> genLovelace
  -- ^ fee
  <*> genInt64
  -- ^ deposit
  <*> genUInteger
  -- ^ size
  <*> Gen.maybe (Db.DbWord64 <$> genWord64)
  -- ^ invalid before (slot)
  <*> Gen.maybe (Db.DbWord64 <$> genWord64)
  -- ^ invalid after (slot)
  <*> Gen.bool
  -- ^ script validity
  <*> genUInteger
  -- ^ script size


genTransaction
  :: ( MonadGen m
     , MonadState [Word32] m
     )
  => m (Transaction 'Ephemeral)
genTransaction =
  TransactionE <$> genTx <*> genBlock <*> genSlotLeader

genVoteRegistration
  :: ( MonadGen m
     , MonadState [Word32] m
     , MonadIO m
     )
  => VoteSigningKey
  -> m (Registration 'Ephemeral)
genVoteRegistration skey = do
  Registration
    <$> Gen.votingKeyPublic
    <*> Gen.rewardsAddress
    <*> genUInteger -- slotNo
    <*> pure skey
    <*> Gen.bool
    <*> genTransaction

genStakeAddressRegistration
  :: ( MonadGen m
     , MonadState [Word32] m
     , MonadIO m
     )
  => m (StakeRegistration 'Ephemeral)
genStakeAddressRegistration = do
  stakingKey           <- Gen.voteSigningKey
  stakeRegoTransaction <- genTransaction
  stakeAddress         <- genStakeAddress

  let
    verKey = getVoteVerificationKey stakingKey
    verKeyHashRaw = voteVerificationKeyStakeAddressHashRaw Cardano.Mainnet verKey
    verKeyView = serialiseVoteVerificationKeyToBech32 verKey

  pure $
    StakeRegistrationE
      stakingKey
      stakeRegoTransaction
      (stakeAddress { Db.stakeAddressHashRaw = verKeyHashRaw
                    , Db.stakeAddressView = verKeyView
                    }
      )

genStakeAddress :: (MonadGen m, MonadState [Word32] m) => m Db.StakeAddress
genStakeAddress =
  Db.StakeAddress
  <$> genUniqueHash32
  <*> Gen.text (Range.linear 0 256) Gen.unicodeAll
  <*> pure Nothing
  <*> (Persist.toSqlKey <$> genInt64)

genStakeAddressForVerificationKey
  :: MonadGen m
  => VoteVerificationKey
  -> m Db.StakeAddress
genStakeAddressForVerificationKey verKey = do
  let verKeyHashRaw = voteVerificationKeyStakeAddressHashRaw Cardano.Mainnet verKey
      verKeyView = serialiseVoteVerificationKeyToBech32 verKey

  Db.StakeAddress verKeyHashRaw verKeyView
  <$> pure Nothing
  <*> (Persist.toSqlKey <$> genInt64)

genTxOut :: MonadGen m => m Db.TxOut
genTxOut = Db.TxOut
  <$> (Persist.toSqlKey <$> genInt64)
  -- ^ tx id
  <*> (fromIntegral <$> Gen.int16 (Range.linear 0 (maxBound :: Int16)))
  -- ^ index (uses smallint >= 0)
  <*> Gen.text (Range.linear 0 103) Gen.ascii
  -- ^ address
  <*> genHash32
  -- ^ address raw
  <*> Gen.bool
  -- ^ has script
  <*> Gen.maybe genHash28
  -- ^ Payment credential
  <*> Gen.maybe (Persist.toSqlKey <$> genInt64)
  -- ^ stake address id
  <*> genLovelace
  -- ^ Value
  <*> Gen.maybe genHash32
  -- ^ Data hash

genTxIn :: MonadGen m => m Db.TxIn
genTxIn = Db.TxIn
  <$> (Persist.toSqlKey <$> genInt64)
  -- ^ Tx in id
  <*> (Persist.toSqlKey <$> genInt64)
  -- ^ Tx out id
  <*> genWord16
  -- ^ Tx out index
  <*> pure Nothing
  -- ^ Redeemer id

genUTxO
  :: ( MonadGen m
     , MonadState [Word32] m
     )
  => m (UTxO 'Ephemeral)
genUTxO = UTxOE
  <$> genTxOut
  <*> genTransaction

genGraph
  :: ( MonadGen m
     , MonadState [Word32] m
     , MonadIO m
     )
  => m (Graph 'Ephemeral)
genGraph = do
  stakeRego <- genStakeAddressRegistration

  Graph stakeRego
    <$> Gen.list (Range.linear 0 3) (genVoteRegistration (stakeRegoKey stakeRego))
    <*> Gen.list (Range.linear 0 3) genUTxO
