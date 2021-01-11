{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Efficient encoding for the Registration algebra.

module Registration.Efficient
  ( -- * Observations
    registry
  , getRegistration
  , isRegistered
  , isNotRegistered
    -- * Constructors
  , register
  , deregister
    -- * Types
  , Registry
  ) where

import Data.List (find, delete, sort)
import Data.Traversable (for)
import Data.Maybe (maybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Registry id a = Registry (Map id a)
  deriving (Show, Functor)

instance (Ord id, Ord a) => Eq (Registry id a) where
  (==) x y = registry x == registry y

instance (Ord id, Ord a) => Monoid (Registry id a) where
  mempty = Registry $ mempty

instance (Ord id, Ord a) => Semigroup (Registry id a) where
  (<>) existingRegos (Registry r) =
    let
      newRegistrations :: [(id, a)]
      newRegistrations = M.toList r
    in
      foldl (\existing new@(ident, x) -> register ident x existing) existingRegos newRegistrations

register :: (Ord id, Ord a) => id -> a -> Registry id a -> Registry id a
register ident x (Registry r) =
  let
    delta = case M.lookup ident r of
      Nothing -> M.insert ident x
      Just x2 -> case compare x x2 of
        -- New registration is older, take existing registration
        LT -> id
        -- New registration is newer, take new registration
        -- OR New registration and existing registration were done at the same time, take new registration (last wins)
        _gte -> M.insert ident x . M.delete ident
  in
    Registry $ delta r

deregister :: Ord id => id -> Registry id a -> Registry id a
deregister ident (Registry r) = Registry $ M.delete ident r

registry :: forall id a. (Ord id, Ord a) => Registry id a -> [(id, a)]
registry (Registry r) = M.toList r

getRegistration :: (Ord id, Ord a) => id -> Registry id a -> Maybe a
getRegistration ident (Registry r) = M.lookup ident r

isRegistered :: (Ord id, Ord a) => id -> Registry id a -> Bool
isRegistered ident (Registry r) = M.member ident r

isNotRegistered :: (Ord id, Ord a) => id -> Registry id a -> Bool
isNotRegistered ident = not . isRegistered ident
