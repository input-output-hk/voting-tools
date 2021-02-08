import           Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Cardano.CLI.Voting.Metadata
import qualified Test.Contribution


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = do
  testGroup "Unit tests"
    [ Test.Contribution.tests
    , Test.Cardano.CLI.Voting.Metadata.tests
    ]
