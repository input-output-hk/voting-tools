module Cardano.Catalyst.Registration
  ( -- * Logic
    isNewer
  , chooseNewer
  , filterLatestRegistrations
  , accumulateRegistrations
  -- * Types
  , module Cardano.Catalyst.Registration.Types
  , module Cardano.Catalyst.Registration.Types.Purpose
  ) where

import           Data.Foldable (foldl')
import           Data.Map.Strict (Map)

import qualified Cardano.Api as Api
import qualified Data.Map.Strict as M

import           Cardano.Catalyst.Registration.Types
import           Cardano.Catalyst.Registration.Types.Purpose

filterLatestRegistrations :: Ord a => [(a, Vote)] -> [Vote]
filterLatestRegistrations regos =
  fmap snd $ M.elems $ foldl' (flip accumulateRegistrations) mempty regos

accumulateRegistrations
  :: Ord a
  => (a, Vote)
  -> Map (Api.Hash Api.StakeKey) (a, Vote)
  -> Map (Api.Hash Api.StakeKey) (a, Vote)
accumulateRegistrations r@(_, rego) =
  let
    stakeHash :: Api.Hash Api.StakeKey
    stakeHash = voteRegistrationStakeHash rego
  in
    M.insertWith chooseNewer stakeHash r

chooseNewer
  :: Ord a
  => (a, Vote) -> (a, Vote) -> (a, Vote)
chooseNewer a b = if b `isNewer` a then b else a

-- | A newer registration will apply over an older one iff the nonce of the new
-- registration is greater than the old.
isNewer
  :: Ord a
  => (a, Vote)
  -> (a, Vote)
  -> Bool
isNewer a@(tA, _regoA) b@(tB, _regoB) =
  let
    (new, old) = if tA > tB then (a, b) else (b, a)

    slotNew = voteRegistrationSlot $ snd new
    slotOld = voteRegistrationSlot $ snd old
  in
    a == (if slotNew > slotOld then new else old)
