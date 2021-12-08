{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Cardano.Api (ShelleyBasedEra(..), TxMetadataJsonSchema (..), metadataToJson, serialiseToCBOR)
import           Cardano.Api.Shelley (ShelleyLedgerEra)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Options.Applicative as Opt
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus
import           Ouroboros.Network.Block (unSlotNo)

import           Cardano.CLI.Voting (createVoteRegistration)
import           Cardano.CLI.Voting.Metadata (voteToTxMetadata)
import qualified Config.Registration as Register

obtainLedgerEraClassConstraints
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> (Consensus.ShelleyBasedEra ledgerera => a) -> a
obtainLedgerEraClassConstraints ShelleyBasedEraShelley f = f
obtainLedgerEraClassConstraints ShelleyBasedEraAllegra f = f
obtainLedgerEraClassConstraints ShelleyBasedEraMary    f = f
obtainLedgerEraClassConstraints ShelleyBasedEraAlonzo  f = f

main :: IO ()
main = do
  regOpts <- Opt.execParser Register.opts
  eCfg    <- runExceptT (Register.mkConfig regOpts)
  case eCfg of
    Left (err :: Register.ConfigError) ->
      fail $ show err
    Right (Register.Config rewardsAddr voteSign votePub slotNo outFormat) -> do
      -- Create a vote registration, encoding our registration
      -- as transaction metadata.
      let
        vote = createVoteRegistration voteSign votePub rewardsAddr (toInteger $ unSlotNo slotNo)
        meta = voteToTxMetadata vote

      case outFormat of
        Register.MetadataOutFormatJSON ->
          liftIO $ LBS.putStr $ Aeson.encode $ metadataToJson TxMetadataJsonNoSchema meta
        Register.MetadataOutFormatCBOR ->
          liftIO $ BSC.putStr $ serialiseToCBOR meta
