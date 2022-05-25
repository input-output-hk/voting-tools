{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Catalyst.Db where

import           Cardano.Catalyst.Crypto (getStakeVerificationKey)
import           Cardano.Catalyst.Query.Types
import           Cardano.Catalyst.Registration (mkVotePayload, signatureMetaKey,
                   votePayloadToTxMetadata)
import           Cardano.Catalyst.Test.DSL (apiToDbMetadata, contributionAmount, genGraph,
                   genStakeAddressRegistration, genTransaction, genUInteger, genUTxO,
                   genVoteRegistration, getGraphVote, getRegistrationVote, getStakeRegoKey,
                   getTxKey, modifyRegistrations, setSlotNo, setStakeAddressRegistrationSlot,
                   setUTxOSlot, signed, stakeRegoKey, unsigned, utxoValue, writeGraph,
                   writeRegistration, writeStakeRego, writeTx, writeUTxO)
import           Cardano.Catalyst.VotePower
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT)
import           Data.Foldable (traverse_)
import           Data.List (sort)
import           Data.Maybe (catMaybes, isJust)
import           Data.Monoid (Sum (..))
import           Data.Pool (Pool)
import           Database.Persist.Postgresql (SqlBackend)
import           Hedgehog (Property, annotate, cover, distributeT, property, (===))
import           Hedgehog.Internal.Property (forAllT)
import           Test.Cardano.Catalyst.Helpers (withinTransaction)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Api
import qualified Cardano.Catalyst.Test.DSL.Gen as Gen
import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Database.Persist.Class as Sql
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: Ord t => Query (ReaderT SqlBackend IO) t -> IO (Pool SqlBackend) -> TestTree
tests intf getConnPool =
  testGroup "Test.Cardano.Catalyst.Db"
    [ testProperty "prop_insert" (prop_insert intf getConnPool)
    , testProperty "prop_nonceRespected" (prop_nonceRespected intf getConnPool)
    , testProperty "prop_unsigned" (prop_unsigned intf getConnPool)
    , testProperty "prop_registerDuplicates" (prop_registerDuplicates intf getConnPool)
    , testProperty "prop_restrictSlotNo" (prop_restrictSlotNo intf getConnPool)
    , testProperty "prop_noRego" (prop_noRego intf getConnPool)
    , testProperty "prop_ignoreDateUTxOStakeRego" (prop_ignoreDateUTxOStakeRego intf getConnPool)
    , testProperty "prop_signatureMalformed" (prop_signatureMalformed intf getConnPool)
    ]

nw :: Cardano.NetworkId
nw = Cardano.Mainnet

-- If we register a key, then make a set of contributions against that
-- registration, the voting power reported should match the sum of the
-- contributions.
prop_insert :: Ord t => Query (ReaderT SqlBackend IO) t -> IO (Pool SqlBackend) -> Property
prop_insert intf getConnPool =
  property $ do
    pool <- liftIO getConnPool

    graph <-
      flip State.evalStateT [1..] $ distributeT $
        forAllT $ genGraph nw

    let
      amt = contributionAmount graph
      mVote = getGraphVote graph

    cover 25 "has valid vote" $ isJust mVote

    withinTransaction pool $ \runQuery -> do
      funds <- runQuery $ do
        _ <- writeGraph graph
        getVoteRegistrationADA intf Cardano.Mainnet Nothing

      case mVote of
        Nothing   ->
          funds === []
        Just vote -> do
          annotate $ "funds: " <> show funds
          annotate $ "expect: " <> show (vote, amt)
          funds === [(vote, amt)]

-- Nonce respected. A newer registration will apply over an older one iff the
-- nonce of the new registration is greater than the old.
prop_nonceRespected :: Ord t => Query (ReaderT SqlBackend IO) t -> IO (Pool SqlBackend) -> Property
prop_nonceRespected intf getConnPool =
  property $ do
    pool <- liftIO getConnPool

    (stakeRego, rego1, rego2) <-
      -- Generate two registrations against the same stake address, with the
      -- same slot number, but different reward addresses.
      -- We expect registration 1 to be respected, because it was the first
      -- registration with the highest nonce.
      flip State.evalStateT [1..] $ distributeT $ forAllT $ do
        stakeRego <- genStakeAddressRegistration nw
        let stakeKey = stakeRegoKey stakeRego
        rego1 <- (signed . setSlotNo 1) <$> genVoteRegistration stakeKey
        rego2 <- (signed . setSlotNo 1) <$> genVoteRegistration stakeKey
        pure (stakeRego, rego1, rego2)

    withinTransaction pool $ \runQuery -> do
      funds <- runQuery $ do
        _ <- writeStakeRego stakeRego
        -- Write registrations to DB in correct order (1 then 2).
        _ <- writeRegistration rego1
        _ <- writeRegistration rego2

        getVoteRegistrationADA intf Cardano.Mainnet Nothing

      let mVote1 = getRegistrationVote rego1
      cover 25 "has valid vote" $ isJust mVote1

      case mVote1 of
        Nothing   ->
          funds === []
        Just vote1 -> do
          annotate $ "funds: " <> show funds
          annotate $ "expect: " <> show (vote1, 0 :: Integer)
          -- Vote 1 is respected
          funds === [(vote1, 0)]


-- Unsigned registrations are never considered valid.
prop_unsigned :: Ord t => Query (ReaderT SqlBackend IO) t -> IO (Pool SqlBackend) -> Property
prop_unsigned intf getConnPool =
  property $ do
    pool <- liftIO getConnPool

    graph <-
      flip State.evalStateT [1..]
      $ distributeT
      $ forAllT (modifyRegistrations (fmap unsigned) <$> genGraph nw)

    withinTransaction pool $ \runQuery -> do
      funds <- runQuery $ do
        _ <- writeGraph graph
        getVoteRegistrationADA intf Cardano.Mainnet Nothing

      funds === []

-- Registering to vote twice should not double your rewards.
prop_registerDuplicates :: Ord t => Query (ReaderT SqlBackend IO) t -> IO (Pool SqlBackend) -> Property
prop_registerDuplicates intf getConnPool =
  property $ do
    pool <- liftIO getConnPool

    (stakeRego, rego1, rego2, utxos) <-
      -- Generate two registrations against the same stake address, with the
      -- same slot number, but different reward addresses.
      -- We expect registration 1 to be respected, because it was the first
      -- registration with the highest nonce.
      flip State.evalStateT [1..] $ distributeT $ forAllT $ do
        stakeRego <- genStakeAddressRegistration nw
        let stakeKey = stakeRegoKey stakeRego
        rego1 <- (signed . setSlotNo 1) <$> genVoteRegistration stakeKey
        rego2 <- (signed . setSlotNo 2) <$> genVoteRegistration stakeKey
        utxos <- Gen.list (Range.linear 0 3) genUTxO
        pure (stakeRego, rego1, rego2, utxos)

    withinTransaction pool $ \runQuery -> do
      funds <- runQuery $ do
        stakeRego' <- writeStakeRego stakeRego
        -- Write registrations to DB
        _ <- writeRegistration rego1
        _ <- writeRegistration rego2
        -- Write utxos to DB
        traverse_ (writeUTxO $ getStakeRegoKey stakeRego') utxos

        getVoteRegistrationADA intf Cardano.Mainnet Nothing

      -- Vote 2 has a higher nonce and so should be respected.
      let mVote2 = getRegistrationVote rego2
      cover 25 "has valid vote" $ isJust mVote2

      case mVote2 of
        Nothing   ->
          funds === []
        Just vote2 -> do
          let expectedValue = getSum $ foldMap (Sum . utxoValue) utxos
          annotate $ "funds: " <> show funds
          annotate $ "expect: " <> show (vote2, expectedValue)
          funds === [(vote2, expectedValue)]

-- Only take registrations before the given slot number.
prop_restrictSlotNo :: Ord t => Query (ReaderT SqlBackend IO) t -> IO (Pool SqlBackend) -> Property
prop_restrictSlotNo intf getConnPool =
  property $ do
    pool <- liftIO getConnPool

    (stakeRegos, regosBefore, regosAfter, slotUpperBound) <-
      flip State.evalStateT [1..] $ distributeT $ forAllT $ do
      slotUpperBound <- genUInteger

      -- It doesn't matter when the stake registration occurs (before or after
      -- slot), only the voting registration slot is important.
      stakeRegos <-
        Set.toList <$> Gen.set (Range.linear 0 32) (genStakeAddressRegistration nw)

      let
        inSlotBound genVoteRego = do
          slot <- Gen.word16 (Range.linear 0 slotUpperBound)
          setSlotNo (fromIntegral slot) <$> genVoteRego

        afterSlotBound genVoteRego = do
          slot <- Gen.word16 (Range.linear (slotUpperBound + 1) maxBound)
          setSlotNo (fromIntegral slot) <$> genVoteRego

      -- One registration for each stake key
      registrationsBefore <-
        traverse (inSlotBound . genVoteRegistration . stakeRegoKey) stakeRegos
      registrationsAfter <-
        traverse (afterSlotBound . genVoteRegistration . stakeRegoKey) stakeRegos

      pure (stakeRegos, registrationsBefore, registrationsAfter, slotUpperBound)

    withinTransaction pool $ \runQuery -> do
      funds <- runQuery $ do
        traverse_ writeStakeRego stakeRegos
        traverse_ writeRegistration (regosBefore <> regosAfter)

        getVoteRegistrationADA
          intf
          Cardano.Mainnet
          (Just $ fromIntegral slotUpperBound)

      let
        validRegosBefore = catMaybes $ getRegistrationVote <$> regosBefore

      sort funds === sort ((\rego -> (rego, 0)) <$> validRegosBefore)

-- Stake address with contributions but no registrations isn't considered.
prop_noRego :: Ord t => Query (ReaderT SqlBackend IO) t -> IO (Pool SqlBackend) -> Property
prop_noRego intf getConnPool =
  property $ do
    pool <- liftIO getConnPool

    (stakeRego, utxos) <-
      flip State.evalStateT [1..] $ distributeT $ forAllT $ do
        stakeRego <- genStakeAddressRegistration nw
        utxos <- Gen.list (Range.linear 0 5) genUTxO
        pure (stakeRego, utxos)

    withinTransaction pool $ \runQuery -> do
      funds <- runQuery $ do
        -- Write a stake address registration to the database
        stakeRego' <- writeStakeRego stakeRego
        -- Write UTxOs to DB that contribute to that stake address
        traverse_ (writeUTxO $ getStakeRegoKey stakeRego') utxos

        getVoteRegistrationADA intf Cardano.Mainnet Nothing

      -- Ensure funds is empty because no registrations were made.
      funds === []

-- TODO the date of the UTxO DOES matter
-- The date of the UTxO and Stake Rego don't matter, as long as they're in the
-- database, only the slot number of the registration transaction is respected.
prop_ignoreDateUTxOStakeRego :: Ord t => Query (ReaderT SqlBackend IO) t -> IO (Pool SqlBackend) -> Property
prop_ignoreDateUTxOStakeRego intf getConnPool =
  property $ do
    pool <- liftIO getConnPool

    (stakeRego, rego, utxos, slotUpperBound) <-
      flip State.evalStateT [1..] $ distributeT $ forAllT $ do
        -- Generate some slot number to restrict the funds query, ensure we
        -- generate a slot number less than the maxBound so we can create
        -- transactions AFTER this slot number.
        slotUpperBound <-
          fromIntegral <$> Gen.int32 (Range.linear 0 (maxBound - 1))
        let afterSlot = slotUpperBound + 1

        -- Generate a stake registration AFTER the slot number.
        stakeRego <-
          setStakeAddressRegistrationSlot afterSlot
          <$> genStakeAddressRegistration nw

        -- Generate some UTxO contributions AFTER the slot number.
        utxos <- fmap (setUTxOSlot afterSlot) <$> Gen.list (Range.linear 0 5) genUTxO

        -- Generate a registration BEFORE or EQUAL to the slot number.
        slot <-
          fromIntegral
          <$> Gen.int32 (Range.linear 0 (fromIntegral slotUpperBound))
        rego <-
          setSlotNo slot
          <$> genVoteRegistration (stakeRegoKey stakeRego)

        pure (stakeRego, rego, utxos, slotUpperBound)

    withinTransaction pool $ \runQuery -> do
      funds <- runQuery $ do
        -- Write a stake address registration to the database
        stakeRego' <- writeStakeRego stakeRego
        -- Write UTxOs to DB that contribute to that stake address
        traverse_ (writeUTxO $ getStakeRegoKey stakeRego') utxos
        -- Write our vote registration
        _ <- writeRegistration rego

        getVoteRegistrationADA
          intf
          Cardano.Mainnet
          (Just $ fromIntegral slotUpperBound)

      let mVote = getRegistrationVote rego
      cover 25 "has valid vote" $ isJust mVote

      case mVote of
        Nothing   ->
          funds === []
        Just vote -> do
          annotate $ "funds: " <> show funds
          annotate $ "expect: " <> show (vote, 0 :: Integer)
          funds === [(vote, 0)]

-- Malformed signatures are never considered valid.
prop_signatureMalformed :: Ord t => Query (ReaderT SqlBackend IO) t -> IO (Pool SqlBackend) -> Property
prop_signatureMalformed intf getConnPool =
  property $ do
    pool <- liftIO getConnPool

    (stakeRego, metaRego, metaSig, regoTx, utxos) <-
      flip State.evalStateT [1..] $ distributeT $ forAllT $ do
        stakeRego <- genStakeAddressRegistration nw
        let stakeKey = stakeRegoKey stakeRego

        -- Gen good registration data
        votePayload <-
          mkVotePayload
          <$> Gen.genVotingKeyPublic
          <*> pure (getStakeVerificationKey stakeKey)
          <*> Gen.genRewardsAddress
          <*> Gen.genSlotNo
        let
          metaRego = votePayloadToTxMetadata votePayload

        -- Gen bad signature data
        let
          metaSig =
            Api.makeTransactionMetadata $ M.singleton
              signatureMetaKey
              (Api.TxMetaMap [ ( Api.TxMetaNumber 1, Api.TxMetaBytes mempty ) ])

        -- Gen transaction to attach
        regoTx <- genTransaction

        -- Gen UTxOs to attach
        utxos <- Gen.list (Range.linear 0 5) genUTxO

        pure (stakeRego, metaRego, metaSig, regoTx, utxos)

    withinTransaction pool $ \runQuery -> do
      funds <- runQuery $ do
        -- Write a stake address registration to the database
        stakeRego' <- writeStakeRego stakeRego

        -- Write UTxOs to DB that contribute to that stake address
        traverse_ (writeUTxO $ getStakeRegoKey stakeRego') utxos

        -- Write transaction to DB
        tx' <- writeTx regoTx

        let
          txId = getTxKey tx'

          dbMeta =
            apiToDbMetadata (metaRego <> metaSig) txId

        -- Associate TxMetadata payload to DB
        traverse_ Sql.insert_ dbMeta

        getVoteRegistrationADA intf Cardano.Mainnet Nothing

      -- Ensure that we get no funds for an invalid signature
      funds === []
