import           Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Cardano.CLI.Voter.Registration


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "VoterRegistration"
    [ Test.Cardano.CLI.Voter.Registration.tests
    ]
