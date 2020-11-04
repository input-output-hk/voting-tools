{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Cardano.Api.Types (Address, Shelley)

data Config
  = Config { networkId             :: NetworkId
           , queryFilter           :: QueryFilter -- this includes the "payment address"
           , protocol              :: Protocol

           , stateDir              :: FilePath
           , paymentSigningKeyFile :: FilePath
           -- , paymentAddress        :: String -- TODO use cardano-api Address
           , votePublicKeyFile     :: FilePath
           , stakeSigningKeyFile   :: SigningKeyFile
           }


defaultByronEpochSlots :: Word64
defaultByronEpochSlots = 21600

-- getVerificationKey :: SigningKeyFile
--                    -> ExceptT ShelleyKeyCmdError IO VerificationKey
-- getVerificationKey skf = do
--     ssk <- firstExceptT ShelleyKeyCmdReadKeyFileError $ readSigningKeyFile skf
--     withSomeSigningKey ssk $ \sk ->
--       pure $ getVerificationKey sk

-- getTip :: Protocol -> NetworkId -> IO String
-- getTip  = capture . . runExceptT . runQueryTip
-- runQueryCmd (QueryTip (CardanoProtocol (EpochSlots 21600)) (networkId) Nothing)
-- runQueryCmd (QueryUTxO protocol queryFilter networkId Nothing)
-- vKey <- getVerificationKey signingKey 

main :: IO ()
main = putStrLn "Hello Haskell!"
