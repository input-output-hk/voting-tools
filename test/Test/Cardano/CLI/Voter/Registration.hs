{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.CLI.Voter.Registration
  ( tests
  )
where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import           Test.HUnit ()
import           Test.QuickCheck (Arbitrary (..), Gen, Property, (=/=), (===), (==>))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

import           Cardano.Api (deserialiseFromRawBytes, deserialiseFromRawBytesHex,
                   makeTransactionMetadata)
import           Cardano.Api.Typed
import           Cardano.Api.Voting (AsType (AsVotingKeyPublic))
import qualified Cardano.Api.Voting as Voting
import           Cardano.CLI.Voting (createVoteRegistration)
import           Cardano.CLI.Voting.Error (AppError)

tests :: TestTree
tests = do
  testCase "Testing generation of vote metadata" $ do
    unitGenerateVoteMetadata

unitGenerateVoteMetadata = do
  (eMetadata :: Either (AppError era) TxMetadata) <- runExceptT $ do
    stkSign   <- maybe (error "Failed to deserialise stake signing key") pure $
      deserialiseFromRawBytesHex (AsSigningKey AsStakeKey) "efb3df51bee6858af75927ce0f4828e55876d8c3111831f87ca4058b7da58a69"
    stkVerify <- maybe (error "Failed to deserialise stake verification key") pure $
      deserialiseFromRawBytesHex (AsVerificationKey AsStakeKey) "08506488dbaa1f0b2918e5d7385ee5d0eacbc72a7e46452e2c463b4f176ab998"
    votePub   <- either (error . show) pure $
      Voting.deserialiseFromBech32 AsVotingKeyPublic "ed25519e_sk1cpxudluugmp8wgl2jrl0hcatlgmgzhwte8cguhqjmq642gzytf3mj05q5f8etx8pv47qadxvsgxjj2pygtf4xglu3emspqt95drxpwg9wqqr4"

    createVoteRegistration stkSign votePub

  case eMetadata of
    Left err   -> assertFailure $ show err
    Right meta -> meta @?= (makeTransactionMetadata $ M.fromList [(1, TxMetaMap
      [ ( TxMetaText "purpose"    , TxMetaText "voting_registration" )
      , ( TxMetaText "signature"  , TxMetaBytes "\145\252\208QQ\244\239\178\ENQ;<\219D\167\a\144\167\201\239\225\179ln\161v\231\216\DC1\SOC{}\204\SOC\133\181\236\167\144\227H\178\&2O\193\189r\192X{\\i\151\253\168\ENQ\205\145\STXe\154.\SO")
      , ( TxMetaText "stake_pub"  , TxMetaBytes "\bPd\136\219\170\US\v)\CAN\229\215\&8^\229\208\234\203\199*~FE.,F;O\ETBj\185\152")
      , ( TxMetaText "voting_key" , TxMetaBytes "\192M\198\255\156F\194w#\234\144\254\251\227\171\250\&6\129]\203\201\240\142\\\DC2\216\&5U DZc\185>\128\162O\149\152\225e|\SO\180\204\130\r)($B\211S#\252\142w\NUL\129e\163F`\185")
      ])])
