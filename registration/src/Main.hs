{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Cardano.Api (TxMetadataJsonSchema (..), metadataToJson, serialiseToCBOR)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Version (showVersion)
import qualified Options.Applicative as Opt
import           Ouroboros.Network.Block (unSlotNo)
import           System.Info (arch, compilerName, compilerVersion, os)

import           Cardano.CLI.Voting (createVoteRegistration)
import           Cardano.CLI.Voting.Metadata (voteToTxMetadata)
import           Paths_voter_registration (version)

import qualified Config.Registration as Register

main :: IO ()
main = do
  regOpts <- Opt.execParser Register.registerCmd
  case regOpts of
    Register.CmdVersion ->
      putStrLn $ mconcat
                [ "voter-registration ", showVersion version
                , " - ", os, "-", arch
                , " - ", compilerName, "-", showVersion compilerVersion
                ]
    Register.CmdRegister regParams -> do
      eCfg    <- runExceptT (Register.mkConfig regParams)
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
