import           Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Cardano.API.Extended
import qualified Test.Cardano.API.Jormungandr
import qualified Test.Cardano.CLI.Voting.Metadata
import qualified Test.Cardano.Catalyst.Crypto
import qualified Test.Cardano.Catalyst.Presentation

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = do
  testGroup "Unit tests"
    [ Test.Cardano.CLI.Voting.Metadata.tests
    , Test.Cardano.API.Extended.tests
    , Test.Cardano.API.Jormungandr.tests
    , Test.Cardano.Catalyst.Presentation.tests
    , Test.Cardano.Catalyst.Crypto.tests
    ]
