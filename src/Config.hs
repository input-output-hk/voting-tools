module Config where

import           Options.Applicative

import qualified Config.Fetch as Fetch
import qualified Config.Registration as Register

data Command
  = Register Register.Opts
  | Genesis Fetch.Opts
  | Rewards Fetch.Opts
  deriving (Eq, Show)

parseOpts :: Parser Command
parseOpts = hsubparser
  (  command "register" (Register <$> Register.opts)
  <> command "genesis"  (Genesis  <$> Fetch.opts)
  <> command "rewards"  (Rewards  <$> Fetch.opts)
  )

opts :: ParserInfo Command
opts =
  info
    ( parseOpts <**> helper )
    ( fullDesc
    <> progDesc "Voting tools"
    <> header "voting-tools - tools to aid in voter registration"
    )
