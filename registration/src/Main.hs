{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Cardano.Api (AnyCardanoEra(AnyCardanoEra), cardanoEraStyle, CardanoEraStyle(ShelleyBasedEra), ShelleyBasedEra(..), shelleyBasedEra, CardanoEra(..))
import           Cardano.Api.Protocol (Protocol (CardanoProtocol), withlocalNodeConnectInfo)
import           Cardano.Api.Modes
import           Cardano.Api.IPC
import           Cardano.Api.Typed (AsType (AsStakeAddress), Hash, Lovelace (Lovelace), ShelleyLedgerEra,
                     StakeCredential (StakeCredentialByKey), StakeKey, makeStakeAddress,
                     serialiseToBech32, serialiseToRawBytesHex)
import           Cardano.Chain.Slotting (EpochSlots (..))
import           Cardano.CLI.Types (QueryFilter (FilterByAddress), SocketPath (SocketPath))
import qualified Cardano.Crypto.DSIGN as Crypto
import           Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (logInfoN, runNoLoggingT, runStderrLoggingT,
                     runStdoutLoggingT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Function ((&))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Options.Applicative as Opt
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

import           Cardano.API.Extended (readEnvSocketPath)
import           Cardano.CLI.Fetching (Fund, chunkFund, fundFromVotingFunds)
import           Cardano.CLI.Voting (createVoteRegistration, encodeVoteRegistration, prettyTx,
                     signTx)
import           Cardano.CLI.Voting.Error (AppError)
import           Cardano.CLI.Voting.Metadata (voteSignature)
import           Cardano.CLI.Voting.Signing (verificationKeyRawBytes)
import           Config
import qualified Config.Genesis as Genesis
import qualified Config.Registration as Register
import qualified Config.Rewards as Rewards
import           Genesis (decodeGenesisTemplateJSON, getBlockZeroDate, setBlockZeroDate,
                     setInitialFunds)

obtainLedgerEraClassConstraints
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> (Consensus.ShelleyBasedEra ledgerera => a) -> a
obtainLedgerEraClassConstraints ShelleyBasedEraShelley f = f
obtainLedgerEraClassConstraints ShelleyBasedEraAllegra f = f
obtainLedgerEraClassConstraints ShelleyBasedEraMary    f = f

main :: IO ()
main = do
  regOpts <- Opt.execParser Register.opts
  eCfg    <- runExceptT (Register.mkConfig regOpts)
  case eCfg of
    Left (err :: Register.ConfigError) ->
      fail $ show err
    Right (Register.Config addr voteSign paySign votePub networkId ttl sign outFile (AnyCardanoEra era) (AnyConsensusModeParams consensusModeParams)) -> do
      eResult <- runExceptT $ do
        SocketPath sockPath <-  readEnvSocketPath
        let connectInfo = LocalNodeConnectInfo consensusModeParams networkId sockPath
        -- Create a vote registration, encoding our registration
        -- as transaction metadata. The transaction sends some
        -- unspent ADA back to us (minus a fee).

        -- Generate vote payload (vote information is encoded as metadata).
        let vote = createVoteRegistration voteSign votePub addr

        -- Encode the vote as a transaction and sign it
        case cardanoEraStyle era of
          ShelleyBasedEra era' -> do
            if sign
              then liftIO $ (writeFile outFile =<<) $ prettyTx . signTx paySign <$> encodeVoteRegistration connectInfo era' addr ttl vote
              else liftIO . putStrLn $ "NOT IMPLEMENTED! Pass `--sign` paramater"

            -- Output helpful information
            liftIO . putStrLn $ "Vote public key used        (hex): " <> BSC.unpack (serialiseToRawBytesHex votePub)
            liftIO . putStrLn $ "Stake public key used       (hex): " <> BSC.unpack (verificationKeyRawBytes voteSign)
            liftIO . putStrLn $ "Vote registration signature (hex): " <> BSC.unpack (Base16.encode . Crypto.rawSerialiseSigDSIGN $ voteSignature vote)
          otherwise            -> error "Byron protocol not supported"

      case eResult of
        Left  (err :: AppError) -> fail $ show err
        Right ()                -> pure ()
