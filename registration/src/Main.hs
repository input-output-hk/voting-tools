{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Cardano.Api (AnyCardanoEra (AnyCardanoEra), AnyConsensusModeParams (..),
                   CardanoEraStyle (ShelleyBasedEra), ChainTip (..), LocalNodeConnectInfo (..),
                   ShelleyBasedEra (..), cardanoEraStyle, getLocalChainTip, serialiseToRawBytesHex)
import           Cardano.Api.Shelley (ShelleyLedgerEra)
import           Cardano.CLI.Types (SocketPath (..))
import qualified Cardano.Crypto.DSIGN as Crypto
import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import qualified Options.Applicative as Opt
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus
import           Ouroboros.Network.Block (unSlotNo)

import           Cardano.API.Extended (readEnvSocketPath)
import           Cardano.CLI.Voting (createVoteRegistration, encodeVoteRegistration, prettyTx,
                   prettyTxBody, signTx)
import           Cardano.CLI.Voting.Error (AppError)
import           Cardano.CLI.Voting.Metadata (voteSignature)
import           Cardano.CLI.Voting.Signing (verificationKeyRawBytes)
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
    Right (Register.Config addr rewardsAddr voteSign paySign votePub networkId ttl sign outFile (AnyCardanoEra era) (AnyConsensusModeParams consensusModeParams)) -> do
      eResult <- runExceptT $ do
        SocketPath sockPath <-  readEnvSocketPath
        let connectInfo = LocalNodeConnectInfo consensusModeParams networkId sockPath
        -- Create a vote registration, encoding our registration
        -- as transaction metadata. The transaction sends some
        -- unspent ADA back to us (minus a fee).

        -- Encode the vote as a transaction and sign it
        case cardanoEraStyle era of
          ShelleyBasedEra era' -> do
            chainTip <- liftIO $ getLocalChainTip connectInfo
            let
                -- Get current slot to embed into vote payload to prevent replay attacks
                slotTip = case chainTip of
                  ChainTipAtGenesis -> 0
                  ChainTip s _ _    -> s
                slotTipInt = toInteger $ unSlotNo slotTip
                -- Generate vote payload (vote information is encoded as metadata).
                vote = createVoteRegistration voteSign votePub rewardsAddr slotTipInt
            if sign
              then liftIO $ (writeFile outFile =<<) $ prettyTx . signTx paySign <$> encodeVoteRegistration connectInfo era' addr ttl vote
              else liftIO $ (writeFile outFile =<<) $ prettyTxBody <$> encodeVoteRegistration connectInfo era' addr ttl vote

            -- Output helpful information
            liftIO . putStrLn $ "Vote public key used        (hex): " <> BSC.unpack (serialiseToRawBytesHex votePub)
            liftIO . putStrLn $ "Stake public key used       (hex): " <> BSC.unpack (Base16.encode . verificationKeyRawBytes $ voteSign)
            liftIO . putStrLn $ "Rewards address used        (hex): " <> BSC.unpack (serialiseToRawBytesHex rewardsAddr)
            liftIO . putStrLn $ "Slot registered:                   " <> show slotTipInt
            liftIO . putStrLn $ "Vote registration signature (hex): " <> BSC.unpack (Base16.encode . Crypto.rawSerialiseSigDSIGN $ voteSignature vote)
          _                    -> error "Byron protocol not supported"

      case eResult of
        Left  (err :: AppError) -> fail $ show err
        Right ()                -> pure ()
