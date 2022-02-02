import           Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Cardano.CLI.Voting.Metadata


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = do
  testGroup "Unit tests"
    [ Test.Cardano.CLI.Voting.Metadata.tests
    ]
