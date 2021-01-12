module Test.Contribution
  ( tests
  ) where

import           Hedgehog (Gen, Property, forAll, property, tripping, (===), MonadTest, annotate)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog
import           Data.List (delete, find)
import           Data.Word (Word8)
import           Test.Tasty.HUnit (Assertion, assertEqual, testCase, (@?=))

import Contribution
import qualified Test.Generators as Gen

tests :: TestTree
tests = testGroup "Contribution algebra"
  [ testGroup "Laws"
      [ testProperty "semigroup/associativity" prop_contributions_semigroup_associativity
      , testProperty "monoid/identity"         prop_contributions_monoid_identity
      , testProperty "monoid/concatenation"    prop_contributions_monoid_concat
      -- , testProperty "register/newest-wins"    prop_contributions_register_newestwins
      -- , testProperty "register/commutative"    prop_contributions_register_commutative
      -- , testProperty "register/deregister"     prop_contributions_register_deregister
      -- , testProperty "deregister/idempotent"   prop_contributions_deregister_idempotent
      -- , testProperty "getRegistration/obs"     prop_contributions_getRegistration
      -- , testProperty "isRegistered/obs"        prop_contributions_isRegistered
      -- , testProperty "isNotRegistered/obs"     prop_contributions_isNotRegistered
      -- , testProperty "functor/identity"        prop_contributions_functor_identity
      -- , testProperty "functor/composition"     prop_contributions_functor_composition
      ]
  , testGroup "Unit tests"
      [ testCase "contribute adds a contribution" unit_contribution_adds
      ]
  ]

unit_contribution_adds :: Assertion
unit_contribution_adds = do
   (contributions $ contribute 1 1 3 mempty) @?= [(1, [(1, 3)])] 
   (contributions $ contribute 1 1 4 $ contribute 1 2 4 mempty) @?= [(1, [(1, 4), (2, 4)])] 

prop_contributions_semigroup_associativity :: Property
prop_contributions_semigroup_associativity = property $ do
  x <- forAll Gen.contributions
  y <- forAll Gen.contributions
  z <- forAll Gen.contributions

  contributions (x <> (y <> z)) === contributions ((x <> y) <> z)

prop_contributions_monoid_identity :: Property
prop_contributions_monoid_identity = property $ do
  x <- forAll Gen.contributions

  -- Right identity
  x <> mempty === x
  -- Left identity
  mempty <> x === x

prop_contributions_monoid_concat :: Property
prop_contributions_monoid_concat = property $ do
  xs <- forAll (Gen.list (Range.linear 0 20) Gen.contributions)

  mconcat xs === foldr (<>) mempty xs

-- prop_contributions_register_newestwins :: Property
-- prop_contributions_register_newestwins = property $ do
--   ident                               <- forAll $ Gen.int (Range.linear 0 maxBound)
--   a1@(Gen.OrderedPayload ord payload) <- forAll Gen.orderedPayload
--   a2 <- forAll $ Gen.frequency [ (1, pure $ Gen.OrderedPayload ord (succ payload))
--                                , (3, Gen.orderedPayload)
--                                ]

--   -- forall contributions in which ident is not registered
--   r  <- deregister ident <$> forAll Gen.contributions

--   register ident a1 (register ident a2 r) === register ident (max a1 a2) r

-- prop_contributions_register_commutative :: Property
-- prop_contributions_register_commutative = property $ do
--   ident <- forAll $ Gen.int (Range.linear 0 maxBound)
--   x    <- forAll Gen.orderedPayload
--   y    <- forAll Gen.orderedPayload

--   -- forall contributions in which ident is not registered
--   r  <- deregister ident <$> forAll Gen.contributions

--   register ident x (register ident y r) === register ident y (register ident x r)

-- prop_contributions_register_deregister :: Property
-- prop_contributions_register_deregister = property $ do
--   ident <- forAll $ Gen.int (Range.linear 0 maxBound)
--   x     <- forAll Gen.orderedPayload
--   r     <- forAll Gen.contributions

--   deregister ident (register ident x r) === r

-- prop_contributions_deregister_idempotent :: Property
-- prop_contributions_deregister_idempotent = property $ do
--   ident <- forAll $ Gen.int (Range.linear 0 maxBound)
--   r     <- forAll Gen.contributions

--   deregister ident (deregister ident r) === deregister ident r
  
-- prop_contributions_getRegistration :: Property
-- prop_contributions_getRegistration = property $ do
--   ident <- forAll $ Gen.int (Range.linear 0 maxBound)
--   r     <- forAll Gen.contributions

--   getRegistration ident r === (fmap snd $ find ((ident ==) . fst) $ contributions r)

-- prop_contributions_isRegistered :: Property
-- prop_contributions_isRegistered = property $ do
--   ident <- forAll $ Gen.int (Range.linear 0 maxBound)
--   r     <- forAll Gen.contributions

--   isRegistered ident r === maybe False (const True) (getRegistration ident r)

-- prop_contributions_isNotRegistered :: Property
-- prop_contributions_isNotRegistered = property $ do
--   ident <- forAll $ Gen.int (Range.linear 0 maxBound)
--   r     <- forAll Gen.contributions

--   isNotRegistered ident r === (not $ isRegistered ident r)

-- prop_contributions_functor_identity :: Property
-- prop_contributions_functor_identity = property $ do
--   r     <- forAll Gen.contributions

--   fmap id r === r

-- prop_contributions_functor_composition :: Property
-- prop_contributions_functor_composition = property $ do
--   r     <- forAll Gen.contributions

--   let
--     f (Gen.OrderedPayload ord payload) = (Gen.OrderedPayload (succ ord) (succ payload))
--     g (Gen.OrderedPayload ord payload) = (Gen.OrderedPayload (succ . succ $ ord) (succ . succ $ payload))

--   fmap (f . g) r === fmap (g . f) r
