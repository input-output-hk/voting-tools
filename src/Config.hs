module Config
  ( module Config.Common
  , Command(..)
  , parseOpts
  , opts
  ) where

import           Data.Foldable (asum)
import           Data.Functor (($>))
import           Options.Applicative

import           Config.Common
import qualified Config.Genesis as Genesis
import qualified Config.Rewards as Rewards

data Command
  = Genesis Genesis.Opts
  | Rewards Rewards.Opts
  | Version
  deriving (Eq, Show)

parseOpts :: Parser Command
parseOpts =
  asum [ pVersionParams $> Version
       , hsubparser
         (  command "genesis"  (Genesis  <$> Genesis.opts)
         <> command "rewards"  (Rewards  <$> Rewards.opts)
         )
       ]

opts :: ParserInfo Command
opts =
  info
    ( parseOpts <**> helper )
    ( fullDesc
    <> progDesc "Voting tools"
    <> header "voting-tools - tools to aid in voter registration"
    )
