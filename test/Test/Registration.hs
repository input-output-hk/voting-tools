module Test.Registration
  ( tests
  ) where

import           Data.List (delete, find)
import           Hedgehog (Gen, MonadTest, Property, annotate, forAll, property, tripping, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog

import           Registration
import qualified Test.Generators as Gen

tests :: TestTree
tests = testGroup "Registration algebra"
  [ testGroup "Laws"
      [ testProperty "semigroup/associativity" prop_registry_semigroup_associativity
      , testProperty "monoid/identity"         prop_registry_monoid_identity
      , testProperty "monoid/concatenation"    prop_registry_monoid_concat
      , testProperty "commutative-monoid"      prop_registry_commutative_monoid
      , testProperty "register/newest-wins"    prop_registry_register_newestwins
      , testProperty "register/commutative"    prop_registry_register_commutative
      , testProperty "register/deregister"     prop_registry_register_deregister
      , testProperty "deregister/idempotent"   prop_registry_deregister_idempotent
      , testProperty "getRegistration/obs"     prop_registry_getRegistration
      , testProperty "isRegistered/obs"        prop_registry_isRegistered
      , testProperty "isNotRegistered/obs"     prop_registry_isNotRegistered
      , testProperty "functor/identity"        prop_registry_functor_identity
      , testProperty "functor/composition"     prop_registry_functor_composition
      ]
  ]

prop_registry_semigroup_associativity :: Property
prop_registry_semigroup_associativity = property $ do
  x <- forAll Gen.registry
  y <- forAll Gen.registry
  z <- forAll Gen.registry

  x <> (y <> z) === (x <> y) <> z

prop_registry_monoid_identity :: Property
prop_registry_monoid_identity = property $ do
  x <- forAll Gen.registry

  -- Right identity
  x <> mempty === x
  -- Left identity
  mempty <> x === x

prop_registry_monoid_concat :: Property
prop_registry_monoid_concat = property $ do
  xs <- forAll (Gen.list (Range.linear 0 20) Gen.registry)

  mconcat xs === foldr (<>) mempty xs

prop_registry_commutative_monoid :: Property
prop_registry_commutative_monoid = property $ do
  x <- forAll Gen.registry
  y <- forAll Gen.registry

  x <> y === y <> x

prop_registry_register_newestwins :: Property
prop_registry_register_newestwins = property $ do
  ident                               <- forAll $ Gen.int (Range.linear 0 maxBound)
  a1@(Gen.OrderedPayload ord payload) <- forAll Gen.orderedPayload
  a2 <- forAll $ Gen.frequency [ (1, pure $ Gen.OrderedPayload ord (succ payload))
                               , (3, Gen.orderedPayload)
                               ]

  -- forall registry in which ident is not registered
  r  <- deregister ident <$> forAll Gen.registry

  register ident a1 (register ident a2 r) === register ident (max a1 a2) r

prop_registry_register_commutative :: Property
prop_registry_register_commutative = property $ do
  ident <- forAll $ Gen.int (Range.linear 0 maxBound)
  x    <- forAll Gen.orderedPayload
  y    <- forAll Gen.orderedPayload

  -- forall registry in which ident is not registered
  r  <- deregister ident <$> forAll Gen.registry

  register ident x (register ident y r) === register ident y (register ident x r)

prop_registry_register_deregister :: Property
prop_registry_register_deregister = property $ do
  ident <- forAll $ Gen.int (Range.linear 0 maxBound)
  x     <- forAll Gen.orderedPayload
  r     <- forAll Gen.registry

  deregister ident (register ident x r) === r

prop_registry_deregister_idempotent :: Property
prop_registry_deregister_idempotent = property $ do
  ident <- forAll $ Gen.int (Range.linear 0 maxBound)
  r     <- forAll Gen.registry

  deregister ident (deregister ident r) === deregister ident r

prop_registry_getRegistration :: Property
prop_registry_getRegistration = property $ do
  ident <- forAll $ Gen.int (Range.linear 0 maxBound)
  r     <- forAll Gen.registry

  getRegistration ident r === (fmap snd $ find ((ident ==) . fst) $ registry r)

prop_registry_isRegistered :: Property
prop_registry_isRegistered = property $ do
  ident <- forAll $ Gen.int (Range.linear 0 maxBound)
  r     <- forAll Gen.registry

  isRegistered ident r === maybe False (const True) (getRegistration ident r)

prop_registry_isNotRegistered :: Property
prop_registry_isNotRegistered = property $ do
  ident <- forAll $ Gen.int (Range.linear 0 maxBound)
  r     <- forAll Gen.registry

  isNotRegistered ident r === (not $ isRegistered ident r)

prop_registry_functor_identity :: Property
prop_registry_functor_identity = property $ do
  r     <- forAll Gen.registry

  fmap id r === r

prop_registry_functor_composition :: Property
prop_registry_functor_composition = property $ do
  r     <- forAll Gen.registry

  let
    f (Gen.OrderedPayload ord payload) = (Gen.OrderedPayload (succ ord) (succ payload))
    g (Gen.OrderedPayload ord payload) = (Gen.OrderedPayload (succ . succ $ ord) (succ . succ $ payload))

  fmap (f . g) r === fmap (g . f) r
