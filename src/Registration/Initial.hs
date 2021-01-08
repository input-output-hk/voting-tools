{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Initial encoding for the Registration algebra.

module Registration.Initial
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
import Data.Maybe (maybe)

data Registry id a where
  Empty      :: Registry id a
  Append     :: Registry id a -> Registry id a -> Registry id a
  Register   :: id -> a -> Registry id a -> Registry id a
  Deregister :: id -> Registry id a -> Registry id a
  Fmap       :: (a -> b) -> Registry id a -> Registry id b

instance (Show id, Show a, Ord id, Ord a) => Show (Registry id a) where
  show Empty                 = "mempty"
  show (Append r1 r2)        = "( " <> show r1 <> " ) <> ( " <> show r2 <> " )"
  show (Register ident x rs) = "register (" <> show ident <> ") (" <> show x <> ") (" <> show rs <> ")"
  show (Deregister ident rs) = "deregister (" <> show ident <> ") (" <> show rs <> ")"
  show (Fmap fn rs)          = "fmap _ _" 
  -- show rs = show $ registry rs

instance (Ord id, Ord a) => Eq (Registry id a) where
  (==) x y = registry x == registry y

register :: id -> a -> Registry id a -> Registry id a
register = Register

deregister :: id -> Registry id a -> Registry id a
deregister = Deregister

registry :: forall id a. (Ord id, Ord a) => Registry id a -> [(id, a)]
registry (Empty)        = []
registry (Append r1 r2) =
  -- Essentially we move all the registrations on the RHS to the LHS,
  -- then get the registrations of the LHS. We're treating everything
  -- on the RHS as happening "after" everything on the LHS.
  let
    newRegistrations = registry r2
    existingRegistry = r1

    newRegistry :: Registry id a
    newRegistry = 
      foldl (\existing (ident, x) -> register ident x existing) existingRegistry newRegistrations 
  in
    sort $ registry newRegistry
registry (Register ident x rs) =
  let
    registered = registry rs

    delta = case findByIndex ident registered of
      Nothing           -> ((ident, x) :)
      Just (ident2, x2) -> case compare x x2 of
        -- New registration is older, take existing registration
        LT -> id
        -- New registration is newer, take new registration
        -- OR New registration and existing registration were done at the same time, take new registration (last wins)
        ord -> ((ident, x) :) . deleteByIndex ident2
  in
    sort $ delta registered
registry (Deregister ident rs) =
  let
    registered = registry rs

    delta = case findByIndex ident registered of
      Nothing          -> id
      Just (ident2, _) -> deleteByIndex ident2
  in
    sort $ delta registered
registry (Fmap f Empty)                 = []
registry (Fmap f (Register ident x rs)) = registry $ Register ident (f x) (Fmap f rs)
registry (Fmap f (Deregister ident rs)) = registry $ Deregister ident (Fmap f rs)
registry (Fmap f (Append r1 r2))        = registry $ Append (Fmap f r1) (Fmap f r2)
registry (Fmap f1 (Fmap f2 rs))         = registry $ Fmap (f1 . f2) rs

getRegistration :: (Ord id, Ord a) => id -> Registry id a -> Maybe a
getRegistration ident = fmap snd . findByIndex ident . registry

isRegistered :: (Ord id, Ord a) => id -> Registry id a -> Bool
isRegistered ident = maybe False (const True) . getRegistration ident

isNotRegistered :: (Ord id, Ord a) => id -> Registry id a -> Bool
isNotRegistered ident = not . isRegistered ident

instance Semigroup (Registry id a) where
  (<>) = Append

instance Monoid (Registry id a) where
  mempty = Empty

instance Functor (Registry id) where
  fmap f rs = Fmap f rs

findByIndex :: Eq id => id -> [(id, a)] -> Maybe (id, a)
findByIndex ident = find ((== ident) . fst)

deleteByIndex :: (Eq id, Eq a) => id -> [(id, a)] -> [(id, a)]
deleteByIndex ident rs =
  case findByIndex ident rs of
    Nothing   -> rs
    Just elem -> delete elem rs
