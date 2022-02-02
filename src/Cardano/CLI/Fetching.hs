{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.CLI.Fetching where

import           Cardano.CLI.Voting.Metadata
import           Cardano.CLI.Voting.Signing
import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import           Data.Char (toLower)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Scientific (toBoundedInteger)
import           Data.Text (Text)
import           GHC.Generics

import           Cardano.Api (Lovelace (Lovelace))
import qualified Cardano.Api as Api

import           Cardano.API.Extended (VotingKeyPublic, deserialiseFromBech32', serialiseToBech32')
import qualified Cardano.API.Jormungandr as Jormungandr

type Threshold = Api.Lovelace

data VotingFunds
  = VotingFunds { _votingFunds :: Map Jormungandr.Address Lovelace }
  deriving (Eq, Show)

instance Semigroup VotingFunds where
  (VotingFunds a) <> (VotingFunds b) = VotingFunds (a <> b)

instance Monoid VotingFunds where
  mempty = VotingFunds mempty

instance FromJSON VotingFunds where
    parseJSON = Aeson.withObject "VotingFunds" $ \v -> do
      let
        decodeLovelace = Aeson.withScientific "Lovelace" $ \n ->
          case toBoundedInteger n of
            Nothing  -> fail $ "failed to convert " <> show n <> " into a valid integer."
            Just num -> pure $ fromInteger $ toInteger (num :: Int64)

        decodeJormungandrAddr k = case deserialiseFromBech32' Jormungandr.AsAddress k of
          Left err    -> fail $ show err
          Right jaddr -> pure jaddr

        kvs = HM.toList v

      kvs' <- traverse (\(k, v') -> (,) <$> decodeJormungandrAddr k <*> decodeLovelace v') kvs
      pure . VotingFunds . M.fromList $ kvs'

instance ToJSON VotingFunds where
  toJSON (VotingFunds m) =
    let
      kvs = M.toList m

      obj :: HashMap Text Aeson.Value
      obj = HM.fromList $ fmap (\(k,v) -> (serialiseToBech32' k, Aeson.toJSON v)) kvs
    in
      Aeson.Object $ obj

aboveThreshold :: Threshold -> VotingFunds -> VotingFunds
aboveThreshold threshold (VotingFunds m) = VotingFunds $ M.filter (\votingPower -> votingPower > threshold) m

fundFromVotingFunds :: VotingFunds -> Fund
fundFromVotingFunds (VotingFunds m) = Fund . fmap (\(addr, (Lovelace val)) -> FundItem addr (fromIntegral val)) . M.toList $ m

scaleFund :: Int -> Fund -> Fund
scaleFund scale (Fund fs) = Fund $ fmap (\(FundItem addr val) -> FundItem addr (val `div` scale)) fs

scaleRegistrationAmt :: Int -> [(a, Integer)] -> [(a, Integer)]
scaleRegistrationAmt scale = fmap (fmap (`div` fromIntegral scale))

data RegistrationInfo
  = RegistrationInfo { _regoInfoRego :: Vote
                     , _regoInfoAmount :: Integer
                     }
  deriving (Eq, Show)

votingPowerFromRegistrationInfo :: Int -> RegistrationInfo -> VotingPower
votingPowerFromRegistrationInfo scale (RegistrationInfo rego amt) =
  let
    power = (amt `div` fromIntegral scale)

    votePub = voteRegistrationPublicKey rego
    stakePub = voteRegistrationVerificationKey rego
    rewardAddr = voteRegistrationRewardsAddress rego
  in
    VotingPower votePub stakePub rewardAddr power

data VotingPower
  = VotingPower { _powerVotingPublicKey :: VotingKeyPublic
                , _powerStakePublicKey :: VoteVerificationKey
                , _powerRewardAddress :: RewardsAddress
                , _powerVotingPower:: Integer
                }
  deriving (Eq, Show, Generic)

votingPowerJsonParserOptions :: Aeson.Options
votingPowerJsonParserOptions = Aeson.defaultOptions
    { Aeson.fieldLabelModifier = fmap toLower . Aeson.camelTo2 '_' . (drop 6) }

instance ToJSON VotingPower where
  toJSON = Aeson.genericToJSON votingPowerJsonParserOptions

instance FromJSON VotingPower where
  parseJSON = Aeson.genericParseJSON votingPowerJsonParserOptions

data FundItem
  = FundItem { fiAddress :: Jormungandr.Address
             , fiValue   :: Int
             }
  deriving (Eq, Show, Ord, Generic)

instance ToJSON FundItem where
  toJSON = Aeson.genericToJSON jsonParserOptions

instance FromJSON FundItem where
  parseJSON = Aeson.genericParseJSON jsonParserOptions

data Fund = Fund { fuFund :: [FundItem] }
  deriving (Eq, Show, Ord, Generic)

instance ToJSON Fund where
  toJSON = Aeson.genericToJSON jsonParserOptions

instance FromJSON Fund where
  parseJSON = Aeson.genericParseJSON jsonParserOptions

jsonParserOptions :: Aeson.Options
jsonParserOptions = Aeson.defaultOptions { Aeson.fieldLabelModifier = (fmap toLower) . (drop 2)
                                         }
chunkFund :: Int -> Fund -> [Fund]
chunkFund c (Fund fs) = Fund <$> chunk c fs

chunk :: Int -> [a] -> [[a]]
chunk c _
  | c <= 0 = []
chunk _ [] = []
chunk c xs =
  let
    (ch, rest) = splitAt c xs
  in
    [ch] <> chunk c rest
