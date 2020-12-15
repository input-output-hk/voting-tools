{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Performs the bulk of the work creating the vote registration
-- transaction.

module Cardano.CLI.Voting where

import           Control.Monad.Except (MonadError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Text (Text)

import           Cardano.API (Address, AsType (AsPaymentKey, AsStakeKey), Key, LocalNodeConnectInfo,
                     Lovelace (Lovelace), NetworkId, PaymentCredential, PaymentKey, SigningKey,
                     StakeAddressReference, StakeCredential, StakeKey, TTL, Tx, TxBody,
                     TxIn (TxIn), TxMetadata, VerificationKey, deserialiseFromRawBytesHex,
                     estimateTransactionFee, getVerificationKey, localNodeNetworkId,
                     makeShelleyAddress, makeShelleyKeyWitness, makeShelleyTransaction,
                     makeSignedTransaction, makeTransactionMetadata, serialiseToBech32,
                     serialiseToRawBytes, serialiseToRawBytesHex, txExtraContentEmpty,
                     verificationKeyHash)
import           Cardano.Api.LocalChainSync (getLocalTip)
import           Cardano.Api.Typed (AsType (AsSigningKey),
                     PaymentCredential (PaymentCredentialByKey), Shelley,
                     ShelleyWitnessSigningKey (WitnessPaymentKey), SigningKey (StakeSigningKey),
                     StakeAddressReference (StakeAddressByValue),
                     StakeCredential (StakeCredentialByKey), StandardShelley, TxId (TxId),
                     TxIx (TxIx),
                     TxMetadataValue (TxMetaBytes, TxMetaMap, TxMetaNumber, TxMetaText),
                     TxOut (TxOut), VerificationKey (StakeVerificationKey),
                     deterministicSigningKey, deterministicSigningKeySeedSize, txCertificates,
                     txMetadata, txUpdateProposal, txWithdrawals)
import qualified Cardano.Binary as CBOR
import           Cardano.CLI.Types (QueryFilter (FilterByAddress, NoFilter))
import           Cardano.Crypto.DSIGN.Class
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Seed as Crypto
import qualified Codec.Binary.Bech32 as Bech32
import           Ouroboros.Consensus.Cardano.Block (Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx)
import           Ouroboros.Network.Block (getTipSlotNo)
import           Ouroboros.Network.Point (fromWithOrigin)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (ShowQuery)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)
import qualified Shelley.Spec.Ledger.Coin as Ledger
import qualified Shelley.Spec.Ledger.Keys as Shelley
import           Shelley.Spec.Ledger.PParams (PParams)
import qualified Shelley.Spec.Ledger.PParams as Shelley
import qualified Shelley.Spec.Ledger.Tx as Ledger
import qualified Shelley.Spec.Ledger.UTxO as Ledger

import           Cardano.API.Extended (textEnvelopeToJSON)
import           Cardano.API.Extended (AsBech32DecodeError, AsBech32HumanReadablePartError,
                     AsShelleyQueryCmdLocalStateQueryError, AsType (AsVotingKeyPublic),
                     VotingKeyPublic, deserialiseFromBech32', queryPParamsFromLocalState,
                     queryUTxOFromLocalState)
import           Cardano.CLI.Voting.Error
import           Cardano.CLI.Voting.Fee
import           Cardano.CLI.Voting.Metadata (Vote, VotePayload, mkVotePayload, signVotePayload,
                     voteMetadata)

-- | Create a vote registration payload.
createVote
  :: SigningKey StakeKey
  -> VotingKeyPublic
  -> Vote
createVote stkSign@(StakeSigningKey skey) votepub =
  let
    stkVerify@(StakeVerificationKey (Shelley.VKey vkey)) = getVerificationKey stkSign

    payload     = mkVotePayload votepub stkVerify
    payloadCBOR = CBOR.serialize' payload
    payloadSig  = signDSIGN () payloadCBOR skey
  in
    case verifyDSIGN () vkey payloadCBOR payloadSig of
      Left err ->
        -- This is an error because there should be no reason the
        -- verification fails, given that our verification key is
        -- derived from the signing key.
        error $ "Failed to validate vote payload: " <> show err
      Right () ->
        signVotePayload payload payloadSig

-- | Encode the vote registration payload as a transaction body.
encodeVote
  :: ( MonadIO m
     , MonadError e m
     , AsShelleyQueryCmdLocalStateQueryError e

     , ShowProxy block
     , ShowProxy (ApplyTxErr block)
     , ShowProxy (Query block)
     , ShowProxy (GenTx block)
     , ShowQuery (Query block)
     )
  => LocalNodeConnectInfo mode block
  -> Address Shelley
  -> TTL
  -> Vote
  -> m (TxBody Shelley)
encodeVote connectInfo addr ttl vote = do
  -- Get the network parameters
  pparams <- queryPParamsFromLocalState connectInfo
  let
    meta      = voteMetadata vote
    networkId = localNodeNetworkId connectInfo

  -- Estimate the fee for the transaction
  let
    feeParams = estimateVoteFeeParams networkId pparams meta

  -- Find some unspent funds
  utxos <- queryUTxOFromLocalState (FilterByAddress $ Set.singleton addr) connectInfo
  case findUnspent feeParams (fromShelleyUTxO utxos) of
    Nothing      -> undefined
    Just unspent -> do
      tip <- liftIO $ getLocalTip connectInfo
      let
        slotTip          = fromWithOrigin minBound $ getTipSlotNo tip
        txins            = unspentSources unspent
        (Lovelace value) = unspentValue unspent
        (Lovelace fee)   =
          estimateVoteTxFee
            networkId pparams slotTip txins addr (Lovelace value) meta

      -- Create the vote transaction
      pure $ voteTx addr txins (Lovelace $ value - fee) (slotTip + ttl) (Lovelace fee) meta

-- | Helper for creating a transaction body.
voteTx
  :: Address Shelley
  -> [TxIn]
  -> Lovelace
  -> TTL
  -> Lovelace
  -> TxMetadata
  -> TxBody Shelley
voteTx addr txins (Lovelace value) ttl (Lovelace fee) meta =
 let
   txouts = [TxOut addr (Lovelace value)]
 in
   makeShelleyTransaction txExtraContentEmpty {
                            txCertificates   = [],
                            txWithdrawals    = [],
                            txMetadata       = Just meta,
                            txUpdateProposal = Nothing
                          }
                          ttl
                          (Lovelace fee)
                          txins
                          txouts

-- | Sign a transaction body to create a transaction.
signTx :: SigningKey PaymentKey -> TxBody Shelley -> Tx Shelley
signTx psk txbody =
  let
    witness = makeShelleyKeyWitness txbody (WitnessPaymentKey psk)
  in
    makeSignedTransaction [witness] txbody

-- | Pretty print a transaction.
prettyTx :: Tx Shelley -> String
prettyTx = BSC.unpack . textEnvelopeToJSON Nothing

fromShelleyUTxO :: Ledger.UTxO StandardShelley -> UnspentSources
fromShelleyUTxO = fmap convert . M.assocs . Ledger.unUTxO
  where
    convert :: (Ledger.TxIn StandardShelley, Ledger.TxOut StandardShelley) -> (TxIn, Lovelace)
    convert (txin, Ledger.TxOut _ (Ledger.Coin value)) = (fromShelleyTxIn txin, Lovelace value)

fromShelleyTxIn  :: Ledger.TxIn StandardShelley -> TxIn
fromShelleyTxIn (Ledger.TxIn txid txix) =
    TxIn (fromShelleyTxId txid) (TxIx (fromIntegral txix))

fromShelleyTxId :: Ledger.TxId StandardShelley -> TxId
fromShelleyTxId (Ledger.TxId h) =
    TxId (Crypto.castHash h)
