{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Cardano.Api (AnyCardanoEra (AnyCardanoEra), AnyConsensusModeParams (..),
                   CardanoEraStyle (ShelleyBasedEra), ChainTip (..), LocalNodeConnectInfo (..),
                   ShelleyBasedEra (..), TxMetadataJsonSchema (..), cardanoEraStyle,
                   getLocalChainTip, metadataToJson, serialiseToCBOR)
import           Cardano.Api.Shelley (ShelleyLedgerEra)
import           Cardano.CLI.Types (SocketPath (..))
import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Options.Applicative as Opt
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus
import           Ouroboros.Network.Block (unSlotNo)

import           Cardano.API.Extended (readEnvSocketPath)
import           Cardano.CLI.Voting (createVoteRegistration)
import           Cardano.CLI.Voting.Error (AppError)
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
    Right (Register.Config rewardsAddr voteSign votePub networkId outFormat (AnyCardanoEra era) (AnyConsensusModeParams consensusModeParams)) -> do
      eResult <- runExceptT $ do
        SocketPath sockPath <-  readEnvSocketPath
        let connectInfo = LocalNodeConnectInfo consensusModeParams networkId sockPath
        -- Create a vote registration, encoding our registration
        -- as transaction metadata.

        -- Encode the vote as a transaction and sign it
        case cardanoEraStyle era of
          ShelleyBasedEra _ -> do
            chainTip <- liftIO $ getLocalChainTip connectInfo
            let
                -- Get current slot to embed into vote payload to prevent replay attacks
                slotTip = case chainTip of
                  ChainTipAtGenesis -> 0
                  ChainTip s _ _    -> s
                slotTipInt = toInteger $ unSlotNo slotTip
                -- Generate vote payload (vote information is encoded as metadata).
                vote = createVoteRegistration voteSign votePub rewardsAddr slotTipInt
                meta = voteToTxMetadata vote

            case outFormat of
              Register.MetadataOutFormatJSON ->
                liftIO $ LBS.putStr $ Aeson.encode $ metadataToJson TxMetadataJsonNoSchema meta
              Register.MetadataOutFormatCBOR ->
                liftIO $ BSC.putStr $ serialiseToCBOR meta
          _                    -> error "Byron protocol not supported"

      case eResult of
        Left  (err :: AppError) -> fail $ show err
        Right ()                -> pure ()
