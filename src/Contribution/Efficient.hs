{-# LANGUAGE RankNTypes #-}

module Contribution.Efficient
  ( -- * Observations
    contributions
    -- * Constructors
  , contribute
  , withdraw
    -- * Types
  , Contributions
  ) where

import Data.Traversable (for)
import Data.Maybe (maybe)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M

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
      Just existing -> M.insert cause (M.delete ident existing)
  in
    Contributions $ delta cs
      
    
