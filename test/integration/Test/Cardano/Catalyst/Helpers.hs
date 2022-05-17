{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Catalyst.Helpers where

import           Control.Exception.Lifted (bracket_)
import           Control.Monad.Base (liftBase)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Resource (MonadUnliftIO)
import           Data.Pool (Pool)
import           Database.Persist.Postgresql (IsolationLevel (Serializable), SqlBackend, rawExecute,
                   runSqlPoolNoTransaction)

import qualified Database.Persist.Class as Sql

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
