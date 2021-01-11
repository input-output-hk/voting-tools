{-# LANGUAGE RankNTypes #-}

-- | This module contains an algebra for encoding the semantics of
-- "registrations".
--
-- The laws associated with each constructor define their semantics,
-- but they are loosely described here.
--   - Each identity may only register once, with the "greatest"
--     registration being used, for some definition of "greatest" (we
--     call this law "register/newest-wins").
--   - The order in which registrations are made does not matter.
--   - De-registering twice is the same as de-registering once.
--   - The registry, which contains all registrations, forms a
--     law-abiding semigroup, monoid, commutative monoid, and functor.
--   - Registering then de-registering is the same as not registering
--     at all.
--
-- The constructors are used to construct values in the algebra.
-- The observations are used to "get values out of" the algebra.
-- All observations can be derived from the observation "registry",
-- i.e. "registry" is the denotation of the algebra.

module Registration
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

import qualified Registration.Efficient as R

type Registry = R.Registry

-- | Register an identity and their information.
-- "register/newestwins":
--   register ident a2 (register ident a1 rs)
--   = register ident (max a2 a1) rs
-- "register/commutative":
--   register ident x (register ident y rs)
--   = register ident y (register ident x rs)
-- "register/deregister":
--   deregister ident (register ident info rs)
--   = rs
register :: (Ord id, Ord a) => id -> a -> Registry id a -> Registry id a
register = R.register

-- | Deregister an identity.
-- "register/deregister":
--   deregister ident (register ident info rs) = rs
-- "deregister/idempotent":
--   deregister ident (deregister ident rs) = deregister ident rs
deregister :: Ord id => id -> Registry id a -> Registry id a
deregister = R.deregister

-- | Return all registrations and their info.
registry :: forall id a. (Ord id, Ord a) => Registry id a -> [(id, a)]
registry = R.registry

-- | Return the information associated with a registration.
-- "getRegistration":
--   getRegistration ident rs
--   = fmap snd $ find ((ident ==) . fst) $ registry r
getRegistration :: (Ord id, Ord a) => id -> Registry id a -> Maybe a
getRegistration = R.getRegistration

-- | Returns true if a identity is registered.
-- "isRegistered":
--   isRegistered ident rs
--   = maybe False (const True) (getRegistration ident r)
isRegistered :: (Ord id, Ord a) => id -> Registry id a -> Bool
isRegistered = R.isRegistered

-- | Returns true if an identity is NOT registered.
-- "isNotRegistered":
--   isNotRegistered ident
--   = not . isRegistered ident
isNotRegistered :: (Ord id, Ord a) => id -> Registry id a -> Bool
isNotRegistered = R.isNotRegistered
