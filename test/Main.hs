import           Test.Tasty (TestTree, defaultMain, testGroup)

-- import qualified Test.Cardano.CLI.Fetching
-- import qualified Test.Registration
import qualified Test.Contribution


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = do
  testGroup "Unit tests"
    [ Test.Contribution.tests
    ]
