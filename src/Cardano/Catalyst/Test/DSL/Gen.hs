{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- |

Module      : Cardano.Catalyst.Test.DSL.Gen
Description : Generate terms for the testing DSL.
Maintainer  : sevanspowell
Stability   : experimental

Generators and associated helpers for generating "Cardano.Catalyst.Test.DSL"
terms.

Values generated from this module are intended to be written to a database. The
cardano-db-sync database in particular has constraints that must be satisfied by
these generated types.

Of particular significance are uniqueness constraints. Under normal
circumstances, a list of values generated from a generator are not guaranteed to
be unique. These non-unique values will fail to be written to a database.

We use stateful generators in this module to generate unique values and overcome
these uniqueness constraints. Any generator with a 'MonadState' constraint is a
stateful generator. In particular, see 'genUniqueHash32'.

These stateful generators must be run in the following manner (or similar):

@
    flip 'Control.Monad.State.Strict.evalStateT' [1..] $ 'Hedgehog.distributeT' $ 'Hedgehog.forAllT' $
      'genStakeAddressRegistration'
@
-}

module Cardano.Catalyst.Test.DSL.Gen
  ( -- * Generators
    -- ** DSL Types
    -- | Generators for terms of the DSL.
    genTransaction
  , genUTxO
  , genVoteRegistration
  , genStakeAddressRegistration
  , genGraph
    -- ** Cardano.Db Generators
    -- | Generators for "Cardano.Db"-specific types
  , genSlotLeader
  , genBlock
  , genTx
  , genLovelace
  , genTxMetadata
  , genStakeAddress
  , genStakeAddressForVerificationKey
  , genTxOut
  , genTxIn
  , genUInteger
  , genRewardsAddress
  , genStakeVerificationKey
  , genVotingKeyPublic
  , genVote
  , genVotePayload
  , genSlotNo
  , genPurpose
  , genDelegations
  , genVoteSigningKey
    -- ** Other
  , genUniqueHash32
  , genUniqueHash28
  , genUTCTime
  , genWord64
  , genWord32
  , genWord63
  , genWord16
  , genInt64
  ) where

import           Cardano.API.Extended (VotingKeyPublic (..))
import           Cardano.Api (AsType (AsStakeExtendedKey, AsStakeKey), NetworkId (Mainnet, Testnet),
                   NetworkMagic (..), generateSigningKey, getVerificationKey, verificationKeyHash)
import           Cardano.Catalyst.Crypto (StakeSigningKey, StakeVerificationKey,
                   getStakeVerificationKey, serialiseStakeVerificationKeyToBech32,
                   signingKeyFromStakeExtendedSigningKey, signingKeyFromStakeSigningKey,
                   stakeAddressFromKeyHash, stakeAddressFromVerificationKey,
                   stakeVerificationKeyHash)
import           Cardano.Catalyst.Registration (DelegationWeight, Delegations (..),
                   RewardsAddress (..), Vote, VotePayload, createVoteRegistration, mkVotePayload)
import           Cardano.Catalyst.Registration.Types.Purpose (Purpose, catalystPurpose, mkPurpose)
import           Cardano.Catalyst.Test.DSL.Internal.Types (Graph (..), PersistState (..),
                   Registration (..), StakeRegistration (..), Transaction (..), UTxO (..),
                   stakeRegoKey)
import           Control.Monad (replicateM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Class (MonadState)
import           Data.ByteString (ByteString)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int16, Int64)
import           Data.Proxy (Proxy (..))
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word16, Word32, Word64)
import           Hedgehog (GenBase, MonadGen, fromGenT)

import qualified Cardano.Api as Cardano
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.DSIGN.Ed25519 as Crypto
import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Db as Db
import qualified Control.Monad.State.Class as State
import qualified Data.Aeson as Aeson
import qualified Data.Binary.Put as Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time.Clock.POSIX as Time
import qualified Database.Persist.Sql as Persist
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genSlotLeader :: (MonadGen m, MonadState [Word32] m) => m Db.SlotLeader
genSlotLeader = Db.SlotLeader
  -- Addresses use a 28 byte hash (as do StakeholdIds).
  <$> genUniqueHash28
  <*> pure Nothing
  <*> Gen.text (Range.linear 0 64) Gen.unicodeAll

genBlock
  :: ( MonadGen m
     , MonadState [Word32] m
     )
  => m Db.Block
genBlock = Db.Block
  -- Hash
  <$> genUniqueHash32
  -- Epoch number
  <*> Gen.maybe genUInteger
  -- Slot number
  <*> Gen.maybe genUInteger
  -- Epoch slot no
  <*> Gen.maybe genUInteger
  -- Block no
  <*> Gen.maybe genUInteger
  -- Previous block id
  <*> pure Nothing
  -- Slot leader id
  <*> (Persist.toSqlKey <$> genInt64)
  -- Block size
  <*> genUInteger
  --- Block time
  <*> genUTCTime
  -- Tx count
  <*> genWord64
  -- Protocol major ver
  <*> genWord16
  -- Protocol major ver
  <*> genWord16
  -- vrf key
  <*> Gen.maybe (Gen.text (Range.singleton 65) Gen.ascii)
  -- op cert
  <*> Gen.maybe genHash32
  -- op cert counter
  <*> Gen.maybe genWord63

genTx
  :: ( MonadGen m
     , MonadState [Word32] m
     )
  => m Db.Tx
genTx = Db.Tx
  <$> genUniqueHash32
  -- Block id
  <*> (Persist.toSqlKey <$> genInt64)
  -- Block index
  <*> genUInteger
  -- out_sum
  <*> genLovelace
  -- fee
  <*> genLovelace
  -- deposit
  <*> genInt64
  -- size
  <*> genUInteger
  -- invalid before (slot)
  <*> Gen.maybe (Db.DbWord64 <$> genWord64)
  -- invalid after (slot)
  <*> Gen.maybe (Db.DbWord64 <$> genWord64)
  -- script validity
  <*> Gen.bool
  -- script size
  <*> genUInteger

genLovelace :: MonadGen m => m Db.DbLovelace
genLovelace =
  Db.DbLovelace <$> Gen.integral (Range.linear 0 18446744073709551615)

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

genStakeAddress :: (MonadGen m, MonadState [Word32] m) => m Db.StakeAddress
genStakeAddress =
  Db.StakeAddress
  <$> genUniqueHash32
  <*> Gen.text (Range.linear 0 256) Gen.unicodeAll
  <*> pure Nothing
  <*> (Persist.toSqlKey <$> genInt64)

genStakeAddressForVerificationKey
  :: MonadGen m
  => NetworkId
  -> StakeVerificationKey
  -> m Db.StakeAddress
genStakeAddressForVerificationKey nw verKey = do
  let
    stakeAddr = stakeAddressFromVerificationKey nw verKey
    addrHashRaw = Cardano.serialiseToRawBytesHex stakeAddr
    addrView = Cardano.serialiseToBech32 stakeAddr

  Db.StakeAddress addrHashRaw addrView
  <$> pure Nothing
  <*> (Persist.toSqlKey <$> genInt64)

genTxOut :: MonadGen m => m Db.TxOut
genTxOut = Db.TxOut
  -- tx id
  <$> (Persist.toSqlKey <$> genInt64)
  -- index (uses smallint >= 0)
  <*> (fromIntegral <$> Gen.int16 (Range.linear 0 (maxBound :: Int16)))
  -- address
  <*> Gen.text (Range.linear 0 103) Gen.ascii
  -- address raw
  <*> genHash32
  -- has script
  <*> Gen.bool
  -- Payment credential
  <*> Gen.maybe genHash28
  -- stake address id
  <*> Gen.maybe (Persist.toSqlKey <$> genInt64)
  -- Value
  <*> genLovelace
  -- Data hash
  <*> Gen.maybe genHash32

genTxIn :: MonadGen m => m Db.TxIn
genTxIn = Db.TxIn
  -- Tx in id
  <$> (Persist.toSqlKey <$> genInt64)
  -- Tx out id
  <*> (Persist.toSqlKey <$> genInt64)
  -- Tx out index
  <*> genWord16
  -- Redeemer id
  <*> pure Nothing

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

-- | A stateful generator, generates a unique 32-byte long hash.
--
-- Makes use of a list of 'Data.Word.Word32's. On generation, it takes a
-- 'Data.Word.Word32' from the state (removing it from future runs), and
-- procedurally generates a hash from this 'Data.Word.Word32'.
--
-- The generator ensures that a unique hash is generated for each
-- 'Data.Word.Word32'.
genUniqueHash32
  :: ( MonadGen m
     , MonadState [Word32] m
     )
  => m ByteString
genUniqueHash32 = do
  -- Get a Word32 from our list
  xs <- State.get
  -- Make sure this Word32 is not used again
  State.put $ tail xs
  let
    -- A Word32 is 4 bytes, but we need a 32-byte hash. So replicate the 4
    -- bytes 8 times. This doesn't cover all possible 32-bytes, but it should
    -- still provide decent coverage.
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

genWord64 :: MonadGen m => m Word64
genWord64 = Gen.word64 Range.linearBounded

genWord32 :: MonadGen m => m Word32
genWord32 = Gen.word32 Range.linearBounded

-- | Positive integer in the range [0, 2147483647 ((2 ^ 31) - 1)].
--
-- See documentation for 'Cardano.Catalyst.Test.DSL.Internal.Types.UInteger'.
genUInteger :: (MonadGen m, Num int) => m int
genUInteger = fromIntegral <$> Gen.int32 (Range.linear 0 maxBound)

genWord63 :: MonadGen m => m Word64
genWord63 = Gen.word64 (Range.linear 0 (2 ^ (63 :: Word64)))

genWord16 :: MonadGen m => m Word16
genWord16 = Gen.word16 Range.linearBounded

genInt64 :: MonadGen m => m Int64
genInt64 = Gen.int64 Range.linearBounded

-- | Generate a DSL 'Transaction' term.
--
-- The generated "Cardano.Db" types won't have valid foreign keys until written
-- to the database with 'Cardano.Catalyst.Test.DSL.Internal.Db.writeTx'.
genTransaction
  :: ( MonadGen m
     , MonadState [Word32] m
     )
  => m (Transaction 'Ephemeral)
genTransaction =
  TransactionE <$> genTx <*> genBlock <*> genSlotLeader

-- | Generate a DSL 'Registration' term.
--
-- The generated "Cardano.Db" types won't have valid foreign keys until written
-- to the database with
-- 'Cardano.Catalyst.Test.DSL.Internal.Db.writeRegistration'.
genVoteRegistration
  :: ( MonadGen m
     , MonadState [Word32] m
     , MonadIO m
     )
  => StakeSigningKey
  -> m (Registration 'Ephemeral)
genVoteRegistration skey = do
  Registration
    <$> genDelegations
    <*> genRewardsAddress
    <*> genUInteger -- slotNo
    <*> pure skey
    <*> Gen.frequency [ (1, pure False), (4, pure True) ]
    <*> genTransaction

-- | Generate a DSL 'StakeRegistration' term.
--
-- The generated "Cardano.Db" types won't have valid foreign keys until written
-- to the database with
-- 'Cardano.Catalyst.Test.DSL.Internal.Db.writeStakeRego'.
genStakeAddressRegistration
  :: ( MonadGen m
     , MonadState [Word32] m
     , MonadIO m
     )
  => NetworkId
  -> m (StakeRegistration 'Ephemeral)
genStakeAddressRegistration nw = do
  stakingKey           <- genVoteSigningKey
  stakeRegoTransaction <- genTransaction
  stakeAddress         <- genStakeAddress

  let
    verKey = getStakeVerificationKey stakingKey
    verKeyHashRaw =
      Cardano.serialiseToRawBytes
      $ stakeAddressFromKeyHash nw
      $ stakeVerificationKeyHash verKey
    verKeyView = serialiseStakeVerificationKeyToBech32 verKey

  pure $
    StakeRegistrationE
      stakingKey
      stakeRegoTransaction
      (stakeAddress { Db.stakeAddressHashRaw = verKeyHashRaw
                    , Db.stakeAddressView = verKeyView
                    }
      )

-- | Generate a DSL 'UTxO' term.
--
-- The generated "Cardano.Db" types won't have valid foreign keys until written
-- to the database with
-- 'Cardano.Catalyst.Test.DSL.Internal.Db.writeUTxO'.
genUTxO
  :: ( MonadGen m
     , MonadState [Word32] m
     )
  => m (UTxO 'Ephemeral)
genUTxO = UTxOE
  <$> genTxOut
  <*> genTransaction

-- | Generate a DSL 'Graph' term.
--
-- The generated "Cardano.Db" types won't have valid foreign keys until written
-- to the database with
-- 'Cardano.Catalyst.Test.DSL.Internal.Db.writeGraph'.
genGraph
  :: ( MonadGen m
     , MonadState [Word32] m
     , MonadIO m
     )
  => NetworkId
  -> m (Graph 'Ephemeral)
genGraph nw = do
  stakeRego <- genStakeAddressRegistration nw

  Graph stakeRego
    <$> Gen.list (Range.linear 0 3) (genVoteRegistration (stakeRegoKey stakeRego))
    <*> Gen.list (Range.linear 0 3) genUTxO

genVotingKeyPublic :: MonadGen m => m VotingKeyPublic
genVotingKeyPublic = do
  let
    seedSize = fromIntegral $ Crypto.seedSizeDSIGN $ Proxy @Crypto.Ed25519DSIGN
  seedBytes <- Gen.bytes (Range.singleton seedSize)
  let seed = Crypto.mkSeedFromBytes seedBytes
  let skey = Crypto.genKeyDSIGN seed
  let vkey = Crypto.deriveVerKeyDSIGN skey
  pure (VotingKeyPublic vkey)

genVoteSigningKey :: (MonadGen m, MonadIO m) => m StakeSigningKey
genVoteSigningKey = do
  a <- liftIO $ signingKeyFromStakeSigningKey <$> generateSigningKey AsStakeKey
  b <- liftIO $ signingKeyFromStakeExtendedSigningKey <$> generateSigningKey AsStakeExtendedKey
  Gen.choice [ pure a
             , pure b
             ]

genStakeVerificationKey :: (MonadGen m, MonadIO m) => m StakeVerificationKey
genStakeVerificationKey =
  getStakeVerificationKey <$> genVoteSigningKey

genRewardsAddress :: (MonadGen m, MonadIO m) => m RewardsAddress
genRewardsAddress = do
  signingKey <- liftIO $ generateSigningKey AsStakeKey
  let hashStakeKey = verificationKeyHash . getVerificationKey $ signingKey

  network <- Gen.choice [ Testnet . NetworkMagic <$> Gen.word32 (Range.linear minBound maxBound), pure Mainnet ]

  RewardsAddress <$> (stakeAddressFromKeyHash network <$> pure hashStakeKey)

genSlotNo :: MonadGen m => m Integer
genSlotNo = fromIntegral <$> Gen.word32 Range.constantBounded

genVotePayload :: (MonadGen m, MonadIO m) => m VotePayload
genVotePayload =
  mkVotePayload
    <$> genDelegations
    <*> genStakeVerificationKey
    <*> genRewardsAddress
    <*> genSlotNo
    <*> Gen.maybe genPurpose

genPurpose :: MonadGen m => m Purpose
genPurpose =
  Gen.frequency [ (9, pure catalystPurpose)
                , (1, genOtherPurpose)
                ]
  where
    genOtherPurpose = do
      purposeNum <- fromIntegral <$> Gen.int64 (Range.linear 0 maxBound)
      case mkPurpose purposeNum of
        Left e  -> error . T.unpack $ e
        Right p -> pure p

genVote :: (MonadGen m, MonadIO m) => m Vote
genVote =
  createVoteRegistration <$> genVoteSigningKey <*> genDelegations <*> genRewardsAddress <*> genSlotNo

genDelegationWeight :: MonadGen m => m DelegationWeight
genDelegationWeight = Gen.word32 Range.linearBounded

genDelegations :: MonadGen m => m (Delegations VotingKeyPublic)
genDelegations =
  Gen.frequency [ (1, LegacyDelegation <$> genVotingKeyPublic)
                , (1, fmap Delegations
                    $ Gen.nonEmpty (Range.linear 0 10)
                    $ (,0) <$> genVotingKeyPublic
                  )
                , (2, fmap Delegations
                    $ Gen.nonEmpty (Range.linear 0 10)
                    $ (,) <$> genVotingKeyPublic <*> genDelegationWeight
                  )
             ]
