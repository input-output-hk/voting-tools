{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Handles configuration, which involves parsing command line
-- arguments and reading key files.

module Config.Registration (Config(Config), ConfigError, opts, mkConfig, Opts(Opts), parseOpts, MetadataOutFormat(..)) where

import           Control.Exception.Safe (try)
import           Control.Lens ((#))
import           Control.Lens.TH
import           Control.Monad.Except (ExceptT, MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Foldable (asum)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           Options.Applicative

import           Cardano.Api (AnyConsensusModeParams, Bech32DecodeError, NetworkId, StakeAddress,
                   TextEnvelopeError)
import qualified Cardano.Api as Api
import           Cardano.CLI.Shelley.Key (InputDecodeError)
import           Cardano.CLI.Types (SigningKeyFile (..))
import           Cardano.CLI.Voting.Signing (VoteSigningKey, readVoteSigningKeyFile)

import           Cardano.API.Extended (AsBech32DecodeError (_Bech32DecodeError),
                   AsFileError (_FileIOError, __FileError), AsInputDecodeError (_InputDecodeError),
                   AsType (AsVotingKeyPublic), VotingKeyPublic, deserialiseFromBech32',
                   pConsensusModeParams, pNetworkId, parseStakeAddress, readerFromAttoParser)
import           Cardano.CLI.Voting.Error (AsTextEnvelopeError (_TextEnvelopeError))
import           Config.Common (pCardanoEra)

data Config = Config
    { cfgRewardsAddress      :: StakeAddress
    , cfgVoteSigningKey      :: VoteSigningKey
    , cfgVotePublicKey       :: VotingKeyPublic
    , cfgNetworkId           :: NetworkId
    , cfgOutFormat           :: MetadataOutFormat
    , cfgEra                 :: Api.AnyCardanoEra
    , cfgConsensusModeParams :: AnyConsensusModeParams
    }
    deriving (Show)

data MetadataOutFormat = MetadataOutFormatJSON
                       | MetadataOutFormatCBOR
    deriving (Eq, Show)

data FileErrors = FileErrorInputDecode InputDecodeError
    | FileErrorTextEnvelope TextEnvelopeError
    deriving (Show)

makePrisms ''FileErrors

instance AsInputDecodeError FileErrors where
  _InputDecodeError = _FileErrorInputDecode

instance AsTextEnvelopeError FileErrors where
  _TextEnvelopeError = _FileErrorTextEnvelope

data ConfigError = ConfigFailedToReadFile (Api.FileError FileErrors)
    | ConfigFailedToDecodeBech32 Bech32DecodeError
    deriving (Show)

makePrisms ''ConfigError

instance AsFileError ConfigError FileErrors where
  __FileError = _ConfigFailedToReadFile

instance AsBech32DecodeError ConfigError where
  _Bech32DecodeError = _ConfigFailedToDecodeBech32

mkConfig
  :: Opts
  -> ExceptT ConfigError IO Config
mkConfig (Opts rewardsAddr vpkf vskf networkId outFile era consensusModeParams) = do
  stkSign <- readVoteSigningKeyFile (SigningKeyFile vskf)
  votepk  <- readVotePublicKey vpkf

  pure $ Config rewardsAddr stkSign votepk networkId outFile era consensusModeParams

data Opts = Opts
    { optRewardsAddress      :: StakeAddress
    , optVotePublicKeyFile   :: FilePath
    , optVoteSigningKeyFile  :: FilePath
    , optNetworkId           :: NetworkId
    , optOutFormat           :: MetadataOutFormat
    , optEra                 :: Api.AnyCardanoEra
    , optConsensusModeParams :: AnyConsensusModeParams
    }
    deriving (Show)

parseOpts :: Parser Opts
parseOpts = Opts
  <$> option (readerFromAttoParser parseStakeAddress) (long "rewards-address" <> metavar "STRING" <> help "address associated with rewards (Must be a stake address for MIR Certificate)")
  <*> strOption (long "vote-public-key-file" <> metavar "FILE" <> help "vote key generated by jcli (corresponding private key must be ed25519extended)")
  <*> strOption (long "stake-signing-key-file" <> metavar "FILE" <> help "stake authorizing vote key")
  <*> pNetworkId
  <*> pOutFormat
  <*> pCardanoEra
  <*> pConsensusModeParams

opts :: ParserInfo Opts
opts =
  info
    ( parseOpts <**> helper )
    ( fullDesc
    <> progDesc "Create vote registration metadata"
    <> header "voter-registration - a tool to create vote registration metadata suitable for attaching to a transaction"
    )

stripTrailingNewlines :: Text -> Text
stripTrailingNewlines = T.intercalate "\n" . filter (not . T.null) . T.lines

readVotePublicKey
  :: ( MonadIO m
     , MonadError e m
     , AsFileError e d
     , AsBech32DecodeError e
     )
  => FilePath
  -> m VotingKeyPublic
readVotePublicKey path = do
  result <- liftIO . try $ TIO.readFile path
  raw    <- either (\e -> throwError . (_FileIOError #) $ (path, e)) pure result
  let publicKeyBech32 = stripTrailingNewlines raw
  either (throwError . (_Bech32DecodeError #)) pure $ deserialiseFromBech32' AsVotingKeyPublic publicKeyBech32

pOutFormat :: Parser MetadataOutFormat
pOutFormat = asum
  [ flag' MetadataOutFormatJSON
      ( long "json"
      <> help "Output metadata in JSON format (using the 'NoSchema' TxMetadata JSON format - the default for cardano-cli)"
      )
  , flag' MetadataOutFormatCBOR
      ( long "cbor"
      <> help "Output metadata in binary CBOR format"
      )
  ]
