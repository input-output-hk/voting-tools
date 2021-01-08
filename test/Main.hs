import           Test.Tasty (TestTree, defaultMain, testGroup)

-- import qualified Test.Cardano.CLI.Fetching
import qualified Test.Registration


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = do
  testGroup "Unit tests"
    [ Test.Registration.tests
    ]
