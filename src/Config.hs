module Config
  ( module Config.Common
  , Command(..)
  , parseOpts
  , opts
  ) where

import           Options.Applicative

import           Config.Common
import qualified Config.Genesis as Genesis

data Command
  = Genesis Genesis.Opts
  deriving (Eq, Show)

parseOpts :: Parser Command
parseOpts = hsubparser
  (  command "genesis"  (Genesis  <$> Genesis.opts) )

opts :: ParserInfo Command
opts =
  info
    ( parseOpts <**> helper )
    ( fullDesc
    <> progDesc "Voting tools"
    <> header "voting-tools - tools to aid in voter registration"
    )
