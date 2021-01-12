{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Initial encoding for the Contribution algebra.

module Contribution.Initial
  ( -- * Observations
    contributions
  --   registry
  -- , getRegistration
  -- , isRegistered
  -- , isNotRegistered
    -- * Constructors
  , contribute
  , withdraw
    -- * Types
  , Contributions
  , addContribution
  , foldMapContributions
  , deleteExisting
  ) where

import Data.List (find, delete, sort, foldl', sortOn)
import Data.Traversable (for)
import Data.Maybe (maybe)
import Data.Function ((&))
import Debug.Trace (trace)

data Contributions cause id amt where
  Empty      :: Contributions cause id amt
  Append     :: Contributions cause id amt -> Contributions cause id amt -> Contributions cause id amt
  Contribute :: cause -> id -> amt -> Contributions cause id amt -> Contributions cause id amt
  Withdraw   :: cause -> id -> Contributions cause id amt -> Contributions cause id amt
  Fmap       :: (amt1 -> amt2) -> Contributions cause id amt1 -> Contributions cause id amt2

instance (Show cause, Show id, Show amt) => Show (Contributions cause id amt) where
  show Empty                        = "mempty"
  show (Append x y)                 = "( " <> show x <> " ) <> ( " <> show y <> " )"
  show (Contribute cause ident amt xs) = "contribute (" <> show cause <> ") (" <> show ident <> ") (" <> show amt <> ") (" <> show xs <> ")"
  show (Withdraw cause ident xs)       = "withdraw (" <> show cause <> ") (" <> show ident <> ") (" <> show xs <> ")"
  show (Fmap f xs)                   = "fmap _ _" 

instance (Ord id, Ord cause, Eq amt) => Eq (Contributions cause id amt) where
  (==) x y = contributions x == contributions y

instance Semigroup (Contributions cause id amt) where
  (<>) = Append

instance Monoid (Contributions cause id amt) where
  mempty = Empty

instance Functor (Contributions cause id) where
  fmap f rs = Fmap f rs

contribute :: cause -> id -> amt -> Contributions cause id amt -> Contributions cause id amt
contribute = Contribute

withdraw :: cause -> id -> Contributions cause id amt -> Contributions cause id amt
withdraw = Withdraw

foldMapContributions :: Monoid m => (cause -> id -> amt -> m) -> [(cause, [(id, amt)])] -> m
foldMapContributions f xs =
  (flip foldMap) xs $ \(cause, cs) ->
    let
      f' = f cause
    in
      (flip foldMap) cs $ \(ident, amt) -> f' ident amt


contributions :: forall cause id amt. (Ord cause, Ord id, Eq amt) => Contributions cause id amt -> [(cause, [(id, amt)])]
contributions Empty                                    = trace "empty" []
contributions (Append x Empty)                         = trace "append x empty" $ contributions x
contributions (Append x y)                             =
  let
    new = (foldMapContributions contribute (contributions y) :: Contributions cause id amt -> Contributions cause id amt) $ x

    -- newContributions = contributions y
    -- existingContributions = x

    -- delta :: Contributions cause id amt -> Contributions cause id amt
    -- -- new = foldl' (\existing (c, cs) -> foldl' (\xs (ident, amt) -> contribute c ident amt xs) existing cs) existingContributions newContributions
    -- delta =
    --   (flip foldMap) newContributions $ \(c, cs) ->
    --     (flip foldMap) cs $ \(ident, amt) -> contribute c ident amt


  in
    trace "append x y"
    $ contributions new
    & sortContributions
contributions (Contribute cause ident amt xs)          =
  contributions xs
  & deleteExisting cause ident
  & addContribution cause ident amt
  & sortContributions
contributions (Withdraw cause ident xs)                =
  contributions xs
  & deleteExisting cause ident
  & sortContributions
contributions (Fmap f Empty)                           = []
contributions (Fmap f (Append x y))                    = contributions $ Append (Fmap f x) (Fmap f y)
contributions (Fmap f (Contribute cause ident amt cs)) = contributions $ Contribute cause ident (f amt) (Fmap f cs)
contributions (Fmap f (Withdraw cause ident cs))       = contributions $ Withdraw cause ident (Fmap f cs)
contributions (Fmap f1 (Fmap f2 cs))                   = contributions $ Fmap (f1 . f2) cs

sortContributions :: (Ord cause, Ord id) => [(cause, [(id, amt)])] -> [(cause, [(id, amt)])]
sortContributions = sortOn fst . fmap (fmap (sortOn fst))

deleteExisting :: (Eq cause, Eq id, Eq amt) => cause -> id -> [(cause, [(id, amt)])] -> [(cause, [(id, amt)])]
deleteExisting cause ident =
  foldl' (\acc (c :: cause, cs :: [(id, amt)]) -> 
          if c == cause
          then
            case findContribution ident cs of
              Just x  -> ((c, delete (ident, x) cs) :) acc
              Nothing -> ((c, cs) :) acc
          else
            ((c, cs) :) acc
        ) mempty 

findContribution :: Eq id => id -> [(id, amt)] -> Maybe amt
findContribution ident = fmap snd . find ((== ident) . fst)

-- findContribution :: (Eq cause, Eq id) => cause -> id -> [(cause, [(id, amt)])] -> Maybe amt
-- findContribution cause ident xs = do
--   ((c, cs) :: (cause, [(id, amt)])) <- find ((== cause) . fst) xs
--   ((i, amt) :: (id, amt))           <- find ((== ident) . fst) cs
--   pure amt

addContribution :: Eq cause => cause -> id -> amt -> [(cause, [(id, amt)])] -> [(cause, [(id, amt)])]
addContribution cause ident amt cs =
  case find ((== cause) . fst) cs of
    Nothing -> (cause, [(ident, amt)]):cs
    Just _  -> foldl' (\acc (c :: cause, cs :: [(id, amt)]) ->
                                           if c == cause
                                           then ((c, (ident, amt) : cs) :) acc
                                           else ((c, cs) :) acc
                                        ) mempty cs
