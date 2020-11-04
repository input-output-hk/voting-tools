{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Exception as E

-- import Ouroboros.Network.Magic
--     ( NetworkMagic (..) )
-- import Cardano.Api.Types (getVerificationKey)

-- -- Extern

-- data Config
--   = Config { networkId             :: NetworkId
--            , queryFilter           :: QueryFilter -- this includes the "payment address"
--            , protocol              :: Protocol

--            , stateDir              :: FilePath
--            , paymentSigningKeyFile :: FilePath
--            -- , paymentAddress        :: String -- TODO use cardano-api Address
--            , votePublicKeyFile     :: FilePath
--            , stakeSigningKeyFile   :: SigningKeyFile
--            }


-- defaultByronEpochSlots :: Word64
-- defaultByronEpochSlots = 21600

-- -- getTip :: Protocol -> NetworkId -> IO String
-- -- getTip  = capture . . runExceptT . runQueryTip
-- -- runQueryCmd (QueryTip (CardanoProtocol (EpochSlots 21600)) (networkId) Nothing)
-- -- runQueryCmd (QueryUTxO protocol queryFilter networkId Nothing)
-- -- vKey <- getVerificationKey signingKey 


-- Ledger.txins $ utxo
-- value <- Ledger.balance utxo
-- let txouts = [(paymentAddress, value)]

-- let meta = TxMetaMap [( TxMetaNumber 1, TxMetaMap
--                         [ (TxMetaText "purpose", TxMetaText "voting_registration")
--                         , (TxMetaText "voting_key", TxMetaText "0x{vote}")
--                         , (TxMetaText "stake_pub",  TxMetaText "0x{stake}")
--                         , (TxMetaText "signature", TxMetaText "0x{sig}")
--                         ]
--                       )
--                      ]


-- let txBody = Api.makeShelleyTransaction
--                Api.txExtraContentEmpty {
--                  Api.txCertificates   = [],
--                  Api.txWithdrawals    = [],
--                  Api.txMetadata       = Just meta,
--                  Api.txUpdateProposal = Nothing
--                }
--                ttl
--                0 
--                txins
--                txouts

-- let tx = Api.makeSignedTransaction [] txbody
--     Api.Lovelace fee = Api.estimateTransactionFee
--                          (networkId)
--                          (Shelley._minfeeB pparams) --TODO: do this better
--                          (Shelley._minfeeA pparams)
--                          tx
--                          (length txINs) (length txOuts)
--                          0 0

-- let txouts' = [(paymentAddress, value - fee)]

-- let txBody' = Api.makeShelleyTransaction
--                Api.txExtraContentEmpty {
--                  Api.txCertificates   = [],
--                  Api.txWithdrawals    = [],
--                  Api.txMetadata       = Just meta,
--                  Api.txUpdateProposal = Nothing
--                }
--                ttl
--                fee
--                txins
--                txouts'

-- tx = Api.makeSignedTransaction (byronWitnesses ++ shelleyWitnesses) txbody

-- firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
--   Api.writeFileTextEnvelope txFile Nothing tx

-- -- Convert JCLI key to bytes
-- vote_pk_bytes <- jcliCmd ["key", "to-byte"] (pure votePublicKey)
-- -- Convert key to JCLI
-- stake_sk <- jcliCmd ["key", "from-bytes", "--type", "ed25519" ] (pure stakeSkey)
-- -- JCLI key public
-- stake_pk <- jcliCmd ["key", "to-public"] (pure stake_sk)
-- sig <- jcliCmd [ "key", "sign", "--secret-key", key_file, text_file ]

main :: IO ()
main = putStrLn "Hello Haskell!"
