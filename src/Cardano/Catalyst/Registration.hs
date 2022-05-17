{-# LANGUAGE TemplateHaskell #-}

module Cardano.Catalyst.Registration where

import           Cardano.CLI.Voting.Metadata (MetadataParsingError, Vote, voteFromTxMetadata,
                   voteRegistrationSlot, voteRegistrationStakeHash)
import           Control.Lens ((#))
import           Control.Lens.TH (makeClassyPrisms)
import           Control.Monad.Except (throwError)
import           Data.Foldable (foldl')
import           Data.Map.Strict (Map)

import qualified Cardano.Api as Api
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as M

data ParseRegistrationError
  = ParseFailedToDecodeTxMetadata !Api.TxMetadataJsonError
  | ParseFailedToDecodeVoteRegistration !MetadataParsingError
  deriving (Eq, Show)

makeClassyPrisms ''ParseRegistrationError

parseRegistration
  :: Aeson.Value
  -> Either ParseRegistrationError Vote
parseRegistration rego = do
  voteRegoTxMetadata <-
    handleEither
    (\e -> _ParseFailedToDecodeTxMetadata # e)
    $ Api.metadataFromJson Api.TxMetadataJsonNoSchema rego

  voteRego <-
    handleEither (\e -> _ParseFailedToDecodeVoteRegistration # e)
    $ voteFromTxMetadata voteRegoTxMetadata

  pure voteRego

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

handleEither :: (e -> e') -> Either e x -> Either e' x
handleEither f = either (throwError . f) pure
