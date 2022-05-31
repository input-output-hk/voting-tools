import           Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Cardano.API.Extended
import qualified Test.Cardano.Catalyst.Crypto
import qualified Test.Cardano.Catalyst.Registration

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = do
  testGroup "Unit tests"
    [ Test.Cardano.Catalyst.Registration.tests
    , Test.Cardano.API.Extended.tests
    , Test.Cardano.Catalyst.Crypto.tests
    ]
