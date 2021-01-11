module Registration where

data Registry id a

-- ∀x. isNotRegistered id1 x =>
--   registry (register id a1 (register id a2 x))
--   = registry (register id a2 (register id a1 x))
--   = [(id, max a1 a2)] <> registry x

-- ∀x. getRegistration id x = find ((id ==) . fst) (registry x)

-- ∀x. isRegistered id x = maybe False True $ getRegistration id x
-- ∀id. isNotRegistered id = not . isRegistered id

-- ∀id x.
--   deregister id x
--   = remove ((id ==) . fst) $ registry x

register :: Ord a => id -> a -> Registry id a -> Registry id a
deregister :: id -> Registry id a -> Registry id a

getRegistration :: id -> Registry id a -> Maybe a
isRegistered    :: id -> Registry id a -> Bool
isNotRegistered :: id -> Registry id a -> Bool

registry :: Registry id a -> [(id, a)]

instance Semigroup (Registry id a)
instance Monoid (Registry id a)
instance Functor (Registry id a)
instance Foldable (Registry id a)
instance Traversable (Registry id a)
instance Commutative (Registry id a)
instance Eq (Registry id a)
instance Ord (Registry id a)
