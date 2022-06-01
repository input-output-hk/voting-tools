{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Catalyst.Db where

import           Cardano.CLI.Voting.Metadata (mkVotePayload, signatureMetaKey,
                   votePayloadToTxMetadata)
import           Cardano.CLI.Voting.Signing (getVoteVerificationKey)
import           Cardano.Catalyst.Test.DSL (apiToDbMetadata, contributionAmount, genGraph,
                   genStakeAddressRegistration, genTransaction, genUInteger, genUTxO,
                   genVoteRegistration, getGraphVote, getRegistrationVote, getStakeRegoKey,
                   getTxKey, modifyRegistrations, setSlotNo, setStakeAddressRegistrationSlot,
                   setUTxOSlot, signed, stakeRegoKey, unsigned, utxoValue, writeGraph,
                   writeRegistration, writeStakeRego, writeTx, writeUTxO)
import           Control.Exception.Lifted (bracket_)
import           Control.Monad.Base (liftBase)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Resource (MonadUnliftIO)
import           Data.Foldable (traverse_)
import           Data.List (sort)
import           Data.Maybe (catMaybes, isJust)
import           Data.Monoid (Sum (..))
import           Data.Pool (Pool)
import           Database.Persist.Postgresql (IsolationLevel (Serializable), SqlBackend,
                   SqlPersistT, rawExecute, runSqlConnWithIsolation, runSqlPoolNoTransaction)
import           Hedgehog (Property, annotate, cover, distributeT, property, (===))
import           Hedgehog.Internal.Property (forAllT)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Api
import qualified Cardano.CLI.Query
import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Database.Persist.Class as Sql
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Generators as Gen
-- import qualified Cardano.CLI.DbSyncQuery

import           Cardano.CLI.Fetching (RegistrationInfo (..))

runQueryNoIsolation :: SqlBackend -> SqlPersistT IO a -> IO a
runQueryNoIsolation backend query =
  runSqlConnWithIsolation query backend Serializable

tests :: IO (Pool SqlBackend) -> TestTree
tests getConnPool =
  testGroup "Test.Cardano.Catalyst.Db"
    [ testProperty "prop_insert" (prop_insert getConnPool)
    , testProperty "prop_nonceRespected" (prop_nonceRespected getConnPool)
    , testProperty "prop_unsigned" (prop_unsigned getConnPool)
    , testProperty "prop_registerDuplicates" (prop_registerDuplicates getConnPool)
    , testProperty "prop_restrictSlotNo" (prop_restrictSlotNo getConnPool)
    , testProperty "prop_noRego" (prop_noRego getConnPool)
    , testProperty "prop_ignoreDateUTxOStakeRego" (prop_ignoreDateUTxOStakeRego getConnPool)
    , testProperty "prop_signatureMalformed" (prop_signatureMalformed getConnPool)
    ]

-- Isolate a set of queries to a single transaction. The transaction is rolled
-- back if the enclosing code finishes successfully or throws an exception.
--
-- Provides an appropriate function to the action to run queries. This function
-- can be called zero or many times.
--
-- Importantly this includes ANY exceptions thrown by the code, not just
-- database-related exceptions. This means that failing test code will also
-- cause the transaction to be rolled back.
withinTransaction
  :: forall m b
   . ( MonadBaseControl IO m
     , MonadIO m
     )
  => Pool SqlBackend
  -- ^ Sql connection pool
  -> ((forall a. ReaderT SqlBackend IO a -> m a) -> m b)
  -- ^ Action, given a function to run Sql queries
  -> m b
  -- ^ Result
withinTransaction pool action =
  bracket_
    (liftBase (runQueryNoTransaction pool $ rawExecute "BEGIN" []))
    (liftBase (runQueryNoTransaction pool $ rawExecute "ROLLBACK" []))
    (action $ liftIO . runQueryNoTransaction pool)

-- Run a query without the wrapping transaction. Useful as we want to wrap
-- multiple queries in a single transaction (see @withinTransaction@).
--
-- We want to wrap in a single transaction so that Haskell code exceptions (test
-- failures) cause the transaction to be rolled back. Postgres doesn't support
-- nested transactions so we can't just start a new transaction for each query.
runQueryNoTransaction
  :: forall backend m a
   . ( MonadUnliftIO m
     , Sql.BackendCompatible SqlBackend backend
     )
  => Pool backend
  -> ReaderT backend m a
  -> m a
runQueryNoTransaction backend query =
  runSqlPoolNoTransaction
    query
    backend
    (Just Serializable)
    -- See https://www.postgresql.org/docs/9.5/transaction-iso.html for more
    -- information. Serializable is probably more strict a isolation level than
    -- we need. The logic in this test suite should prevent transactions running
    -- concurrently (see withinTransaction and NumThreads) but a strict
    -- isolation level is only harmful if we need to retry transactions due to
    -- serialization failures. So if we start seeing that, consider changing
    -- this to something looser.

-- If we register a key, then make a set of contributions against that
-- registration, the voting power reported should match the sum of the
-- contributions.
prop_insert :: IO (Pool SqlBackend) -> Property
prop_insert getConnPool =
  property $ do
    pool <- liftIO getConnPool

    graph <-
      flip State.evalStateT [1..] $ distributeT $
        forAllT genGraph

    let
      amt = contributionAmount graph
      mVote = getGraphVote graph

    cover 25 "has valid vote" $ isJust mVote

    withinTransaction pool $ \runQuery -> do
      funds <- runQuery $ do
        _ <- writeGraph graph
        Cardano.CLI.Query.queryVotingFunds Cardano.Mainnet Nothing

      case mVote of
        Nothing   ->
          funds === []
        Just vote -> do
          annotate $ "funds: " <> show funds
          annotate $ "expect: " <> show (RegistrationInfo vote amt)
          funds === [RegistrationInfo vote amt]

-- Nonce respected. A newer registration will apply over an older one iff the
-- nonce of the new registration is greater than the old.
prop_nonceRespected :: IO (Pool SqlBackend) -> Property
prop_nonceRespected getConnPool =
  property $ do
    pool <- liftIO getConnPool

    (stakeRego, rego1, rego2) <-
      -- Generate two registrations against the same stake address, with the
      -- same slot number, but different reward addresses.
      -- We expect registration 1 to be respected, because it was the first
      -- registration with the highest nonce.
      flip State.evalStateT [1..] $ distributeT $ forAllT $ do
        stakeRego <- genStakeAddressRegistration
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

        Cardano.CLI.Query.queryVotingFunds Cardano.Mainnet Nothing

      let mVote1 = getRegistrationVote rego1
      cover 25 "has valid vote" $ isJust mVote1

      case mVote1 of
        Nothing   ->
          funds === []
        Just vote1 -> do
          annotate $ "funds: " <> show funds
          annotate $ "expect: " <> show (RegistrationInfo vote1 0)
          -- Vote 1 is respected
          funds === [RegistrationInfo vote1 0]


-- Unsigned registrations are never considered valid.
prop_unsigned :: IO (Pool SqlBackend) -> Property
prop_unsigned getConnPool =
  property $ do
    pool <- liftIO getConnPool

    graph <-
      flip State.evalStateT [1..]
      $ distributeT
      $ forAllT (modifyRegistrations (fmap unsigned) <$> genGraph)

    withinTransaction pool $ \runQuery -> do
      funds <- runQuery $ do
        _ <- writeGraph graph
        Cardano.CLI.Query.queryVotingFunds Cardano.Mainnet Nothing

      funds === []

-- Registering to vote twice should not double your rewards.
prop_registerDuplicates :: IO (Pool SqlBackend) -> Property
prop_registerDuplicates getConnPool =
  property $ do
    pool <- liftIO getConnPool

    (stakeRego, rego1, rego2, utxos) <-
      -- Generate two registrations against the same stake address, with the
      -- same slot number, but different reward addresses.
      -- We expect registration 1 to be respected, because it was the first
      -- registration with the highest nonce.
      flip State.evalStateT [1..] $ distributeT $ forAllT $ do
        stakeRego <- genStakeAddressRegistration
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

        Cardano.CLI.Query.queryVotingFunds Cardano.Mainnet Nothing

      -- Vote 2 has a higher nonce and so should be respected.
      let mVote2 = getRegistrationVote rego2
      cover 25 "has valid vote" $ isJust mVote2

      case mVote2 of
        Nothing   ->
          funds === []
        Just vote2 -> do
          let expectedValue = getSum $ foldMap (Sum . utxoValue) utxos
          annotate $ "funds: " <> show funds
          annotate $ "expect: " <> show (RegistrationInfo vote2 expectedValue)
          funds === [RegistrationInfo vote2 expectedValue]

-- Only take registrations before the given slot number.
prop_restrictSlotNo :: IO (Pool SqlBackend) -> Property
prop_restrictSlotNo getConnPool =
  property $ do
    pool <- liftIO getConnPool

    (stakeRegos, regosBefore, regosAfter, slotUpperBound) <-
      flip State.evalStateT [1..] $ distributeT $ forAllT $ do
      slotUpperBound <- genUInteger

      -- It doesn't matter when the stake registration occurs (before or after
      -- slot), only the voting registration slot is important.
      stakeRegos <-
        Set.toList <$> Gen.set (Range.linear 0 32) genStakeAddressRegistration

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

        Cardano.CLI.Query.queryVotingFunds
          Cardano.Mainnet
          (Just $ fromIntegral slotUpperBound)

      let
        validRegosBefore = catMaybes $ getRegistrationVote <$> regosBefore

      sort funds === sort ((\rego -> RegistrationInfo rego 0) <$> validRegosBefore)

-- Stake address with contributions but no registrations isn't considered.
prop_noRego :: IO (Pool SqlBackend) -> Property
prop_noRego getConnPool =
  property $ do
    pool <- liftIO getConnPool

    (stakeRego, utxos) <-
      flip State.evalStateT [1..] $ distributeT $ forAllT $ do
        stakeRego <- genStakeAddressRegistration
        utxos <- Gen.list (Range.linear 0 5) genUTxO
        pure (stakeRego, utxos)

    withinTransaction pool $ \runQuery -> do
      funds <- runQuery $ do
        -- Write a stake address registration to the database
        stakeRego' <- writeStakeRego stakeRego
        -- Write UTxOs to DB that contribute to that stake address
        traverse_ (writeUTxO $ getStakeRegoKey stakeRego') utxos

        Cardano.CLI.Query.queryVotingFunds Cardano.Mainnet Nothing

      -- Ensure funds is empty because no registrations were made.
      funds === []

-- TODO the date of the UTxO DOES matter
-- The date of the UTxO and Stake Rego don't matter, as long as they're in the
-- database, only the slot number of the registration transaction is respected.
prop_ignoreDateUTxOStakeRego :: IO (Pool SqlBackend) -> Property
prop_ignoreDateUTxOStakeRego getConnPool =
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
          <$> genStakeAddressRegistration

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

        Cardano.CLI.Query.queryVotingFunds
          Cardano.Mainnet
          (Just $ fromIntegral slotUpperBound)

      let mVote = getRegistrationVote rego
      cover 25 "has valid vote" $ isJust mVote

      case mVote of
        Nothing   ->
          funds === []
        Just vote -> do
          annotate $ "funds: " <> show funds
          annotate $ "expect: " <> show (RegistrationInfo vote 0)
          funds === [RegistrationInfo vote 0]

-- Malformed signatures are never considered valid.
prop_signatureMalformed :: IO (Pool SqlBackend) -> Property
prop_signatureMalformed getConnPool =
  property $ do
    pool <- liftIO getConnPool

    (stakeRego, metaRego, metaSig, regoTx, utxos) <-
      flip State.evalStateT [1..] $ distributeT $ forAllT $ do
        stakeRego <- genStakeAddressRegistration
        let stakeKey = stakeRegoKey stakeRego

        -- Gen good registration data
        votePayload <-
          mkVotePayload
          <$> Gen.votingKeyPublic
          <*> pure (getVoteVerificationKey stakeKey)
          <*> Gen.rewardsAddress
          <*> Gen.slotNo
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

        Cardano.CLI.Query.queryVotingFunds Cardano.Mainnet Nothing

      -- Ensure that we get no funds for an invalid signature
      funds === []
