{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Performs the bulk of the work creating the vote registration
-- transaction.

module Cardano.CLI.Voting where

import           Data.Maybe (fromMaybe)

import           Cardano.Api
import           Cardano.Crypto.DSIGN.Class
import           Cardano.Ledger.Crypto (Crypto (..), StandardCrypto)

import           Cardano.API.Extended (VotingKeyPublic)
import           Cardano.CLI.Voting.Metadata (RewardsAddress, Vote, mkVotePayload, signVotePayload)
import           Cardano.Catalyst.Crypto (StakeSigningKey, getStakeVerificationKey, sign)

-- | Create a vote registration payload.
createVoteRegistration
  :: StakeSigningKey
  -> VotingKeyPublic
  -> RewardsAddress
  -> Integer
  -> Vote
createVoteRegistration skey votepub rewardsAddr slot =
    let
      payload     = mkVotePayload votepub (getStakeVerificationKey skey) rewardsAddr slot
      payloadCBOR = serialiseToCBOR payload

      payloadSig  :: SigDSIGN (DSIGN StandardCrypto)
      payloadSig  = payloadCBOR `sign` skey
  in
    fromMaybe (error "Failed to sign vote payload") $
      signVotePayload payload payloadSig
