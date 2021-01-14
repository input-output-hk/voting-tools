{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Contribution.Efficient
  ( -- * Observations
    contributions
  , sumAmounts
  , causeSumAmounts
  , proportionalize
  , contributionsBy
  , contributionsFor
    -- * Constructors
  , contribute
  , withdraw
    -- * Types
  , Contributions
  ) where

import           Data.List (foldl')
import qualified Data.Map.Merge.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, maybe)
import           Data.Monoid (Sum (Sum), getSum)
import           Data.Ratio
import           Data.Traversable (for)

data Contributions cause id amt = Contributions (Map cause (Map id amt))
   deriving Show

instance (Eq cause, Eq id, Eq amt) => Eq (Contributions cause id amt) where
  (==) x y = contributions x == contributions y

instance (Ord cause, Ord id) => Monoid (Contributions cause id amt) where
  mempty = Contributions mempty

instance (Ord cause, Ord id) => Semigroup (Contributions cause id amt) where
  (<>) (Contributions existing) (Contributions new) =
    Contributions
    $ M.merge
        M.preserveMissing
        M.preserveMissing
        (M.zipWithMatched (\c ->
          M.merge
            M.preserveMissing
            M.preserveMissing
           (M.zipWithMatched (\ident amt1 amt2 -> amt2)
           )
        ))
        existing
        new

instance Foldable (Contributions cause id) where
  foldMap f (Contributions cs) = foldMap (foldMap f) cs

contributions :: Contributions cause id amt -> [(cause, [(id, amt)])]
contributions (Contributions cs) = M.toList . fmap M.toList $ cs

contribute :: forall cause id amt . (Ord cause, Ord id) => cause -> id -> amt -> Contributions cause id amt -> Contributions cause id amt
contribute cause ident amt (Contributions cs) =
  let
    mod = case M.lookup cause cs of
      Nothing       -> M.singleton ident amt
      Just existing -> M.insert ident amt existing
  in
    Contributions $ M.insert cause mod cs

withdraw :: (Ord cause, Ord id) => cause -> id -> Contributions cause id amt -> Contributions cause id amt
withdraw cause ident (Contributions cs) =
  let
    delta = case M.lookup cause cs of
      Nothing       -> id
      Just existing ->
        let
          newIds = M.delete ident existing
        in
          if length newIds == 0
          -- A cause with no ids should be deleted for comparison
          -- purposes i.e. we want to consider [(0, [])] and [] as
          -- "meaning the same thing"/"being the same structure". This
          -- logic perhaps belongs in "contributions" but seems less
          -- expensive here.
          then M.delete cause
          else M.insert cause newIds
  in
    Contributions $ delta cs

-- | Sum all contribution amounts.
--
-- Integer was chosen to prevent Sum overflows. Ratio was chosen to
-- allow fractional types.
sumAmounts
  :: forall cause id amt
   . (Num amt, Real amt)
  => Contributions cause id amt -> Ratio Integer
sumAmounts = getSum . foldMap (Sum . toRational)

-- | Best visualized in terms of numbers, instead of each contribution
-- being a given amount, each contribution is interpreted as a
-- proportion of the total amount. For example,
--   proportionalize (contribute 0 0 1 (contribute 1 1 1 mempty))
--   = [(0, [(0, 0.5)]), (1, [(1, 0.5)])]
-- because each contributor contributed 50% of the total value in the
-- system towards their cause.
proportionalize :: forall cause id amt . (Num amt, Real amt) => Contributions cause id amt -> [(cause, [(id, Ratio Integer)])]
proportionalize cs =
  let
    totalValue = sumAmounts cs

    toRatio amt =
      let
        amtR :: Ratio Integer
        amtR = toRational amt
      in
        if amtR == 0 || totalValue == 0
        then fromInteger 0
        else amtR / totalValue
  in
    fmap (fmap (fmap (fmap toRatio))) $ contributions cs

-- | Get the contributions made by a particular identity.
contributionsBy
  :: Ord id
  => id
  -> Contributions cause id amt
  -> [(cause, amt)]
contributionsBy ident (Contributions m) =
  M.foldMapWithKey (\c cs ->
    case M.lookup ident cs of
      Nothing  -> mempty
      Just amt -> [(c, amt)]
  ) m

-- | Get the contributions made towards a particular cause.
contributionsFor
  :: Ord cause
  => cause
  -> Contributions cause id amt
  -> [(id, amt)]
contributionsFor c (Contributions m) = maybe [] M.toList $ M.lookup c m

causeSumAmounts
  :: forall cause id amt
   . (Num amt, Real amt)
  => Contributions cause id amt
  -> [(cause, Ratio Integer)]
causeSumAmounts =
  let
    sumAmts = getSum . foldMap (Sum . toRational . snd)
  in
    fmap (fmap sumAmts) . contributions

-- -- | Lift the contribution amounts into some monoid, and monoidally
-- -- combine each contribution amount.
-- -- For example,
-- --   foldMapAmounts Sum getSum cs
-- -- will provide the sum of all contribution amounts.
-- combineAmounts
--   :: Monoid m
--   => (amt -> m)
--   -> Contributions cause id amt
--   -> m
-- combineAmounts f =
--   foldMap (\(c, cs) -> eachContribution f cs) . contributions
--   where
--     eachContribution f = foldMap (\(_, amt) -> f amt)
