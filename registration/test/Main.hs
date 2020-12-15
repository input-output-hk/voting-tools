import           Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Cardano.CLI.Fetching


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = do
  testGroup "Voting tools tests"
    [ Test.Cardano.CLI.Fetching.tests
    ]
