{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Catalyst.Query where

import           Cardano.Catalyst.Query.Types
import           Cardano.Catalyst.Test.DSL (genGraph, getStakeAddress,
                   graphStakeAddressRegistration, graphToQuery)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT)
import           Data.Foldable (traverse_)
import           Data.List (sort)
import           Data.Pool (Pool)
import           Database.Persist.Postgresql (SqlBackend)
import           Hedgehog (Property, distributeT, property, (===))
import           Hedgehog.Internal.Property (forAllT)
import           Test.Cardano.Catalyst.Helpers (withinTransaction)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Api
import qualified Control.Monad.State.Strict as State
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: Query (ReaderT SqlBackend IO) t -> IO (Pool SqlBackend) -> TestTree
tests intf getConnPool =
  testGroup "Test.Cardano.Catalyst.Query"
    [
      -- ∀xs. length xs === length (queryStakeValues xs)
      testProperty "prop_stakeValuesLength" (prop_stakeValuesLength intf getConnPool)
      -- ∀xs. queryStakeValues xs === zip stakeAddrs <$> traverse queryStakeValue xs
    , testProperty "prop_stakeValues_stakeValue" (prop_stakeValues_stakeValue intf getConnPool)
    ]

-- | The length of the returned stake values should match the length of stake
-- addresses requested.
--
-- That is, any stake addresses without any value should be given a value of 0.
prop_stakeValuesLength :: Query (ReaderT SqlBackend IO) t -> IO (Pool SqlBackend) -> Property
prop_stakeValuesLength intf getConnPool =
  property $ do
    pool <- liftIO getConnPool

    graphs <-
      flip State.evalStateT [1..] $ distributeT $
        forAllT (Gen.list (Range.linear 0 10) genGraph)

    let
      stakeAddrs :: [Api.StakeAddress]
      stakeAddrs =
          fmap (getStakeAddress Cardano.Mainnet . graphStakeAddressRegistration) graphs

    withinTransaction pool $ \runQuery -> do
      stakeValues <- runQuery $ do
        traverse_ graphToQuery graphs
        queryStakeValues intf Nothing stakeAddrs

      length stakeAddrs === length stakeValues

-- | Querying using 'queryStakeValues' with a list of stake addresses should
-- match querying each individual stake address with 'queryStakeValue'.
prop_stakeValues_stakeValue
  :: Query (ReaderT SqlBackend IO) t
  -> IO (Pool SqlBackend)
  -> Property
prop_stakeValues_stakeValue intf getConnPool =
  property $ do
    pool <- liftIO getConnPool

    graphs <-
      flip State.evalStateT [1..] $ distributeT $
        forAllT (Gen.list (Range.linear 0 10) genGraph)

    let
      stakeAddrs :: [Api.StakeAddress]
      stakeAddrs =
          fmap (getStakeAddress Cardano.Mainnet . graphStakeAddressRegistration) graphs

    withinTransaction pool $ \runQuery -> do
      (stakeValues, stakeValueList) <- runQuery $ do
        traverse_ graphToQuery graphs
        stakeValues <- queryStakeValues intf Nothing stakeAddrs
        stakeValueList <- zip stakeAddrs <$> traverse (queryStakeValue intf Nothing) stakeAddrs
        pure (stakeValues, stakeValueList)

      sort stakeValues === sort stakeValueList
