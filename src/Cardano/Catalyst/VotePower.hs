{-# LANGUAGE ScopedTypeVariables #-}

{- |

Module – Cardano.Catalyst.VotePower
Description – Application to find voting power associated with vote registrations.
Maintainer – sevanspowell
Stability – experimental

This module represents the "application layer" of the voting-tools "find vote
power" executable. It provides a single function that provides the amount of ADA
associated with each of the latest vote registrations available at a particular
point in time.
-}

module Cardano.Catalyst.VotePower where

import           Cardano.Catalyst.Query.Types (Query (..))
import           Cardano.Catalyst.Registration

import qualified Cardano.Api as Api
import qualified Data.Aeson as Aeson

getVoteRegistrationADA
  :: ( Monad m
     , Ord t
     )
  => Query m t
  -> Api.NetworkId
  -> Maybe Api.SlotNo
  -> m [(Vote, Integer)]
getVoteRegistrationADA q nw slotNo = do
  (regosRaw :: [(t, Aeson.Value)]) <- (queryVoteRegistrations q) slotNo

  let
    regos :: [(t, Vote)]
    regos =
      flip foldMap regosRaw $ \(t, regoRaw) -> do
        case parseRegistration regoRaw of
          Left _e    -> []
          Right rego -> [(t, rego)]

  let
    latestRegos :: [Vote]
    latestRegos = filterLatestRegistrations regos

  (regoStakes :: [(Api.StakeAddress, Integer)]) <-
    (queryStakeValues q) slotNo
      $ fmap (voteRegistrationStakeAddress nw) latestRegos

  let
    regoValues :: [(Vote, Integer)]
    regoValues = (zip latestRegos . fmap snd) regoStakes

  pure regoValues
