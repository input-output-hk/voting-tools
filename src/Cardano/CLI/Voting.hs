{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Performs the bulk of the work creating the vote registration
-- transaction.

module Cardano.CLI.Voting where

import qualified Data.ByteString.Char8 as BSC
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import           Cardano.Api
import           Cardano.Crypto.DSIGN.Class
import           Cardano.Ledger.Crypto (Crypto (..), StandardCrypto)
import           Ouroboros.Consensus.Cardano.Block (EraMismatch)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)

import           Cardano.API.Extended (VotingKeyPublic, liftShelleyBasedMetadata,
                   liftShelleyBasedTxFee, textEnvelopeToJSON)
import           Cardano.CLI.Voting.Fee
import           Cardano.CLI.Voting.Metadata (RewardsAddress, Vote, mkVotePayload, signVotePayload,
                   voteToTxMetadata)
import           Cardano.CLI.Voting.Signing (VotePaymentKey, VoteSigningKey, getVoteVerificationKey,
                   sign, withWitnessPaymentKey)

-- | Create a vote registration payload.
createVoteRegistration
  :: VoteSigningKey
  -> VotingKeyPublic
  -> RewardsAddress
  -> Integer
  -> Vote
createVoteRegistration skey votepub rewardsAddr slot =
    let
      payload     = mkVotePayload votepub (getVoteVerificationKey skey) rewardsAddr slot
      payloadCBOR = serialiseToCBOR payload

      payloadSig  :: SigDSIGN (DSIGN StandardCrypto)
      payloadSig  = payloadCBOR `sign` skey
  in
    fromMaybe (error "Failed to sign vote payload") $
      signVotePayload payload payloadSig

handleEraMismatch :: Either EraMismatch a -> IO a
handleEraMismatch (Left eraMismatch) = error $ "Era mismatch error: " <> show eraMismatch
handleEraMismatch (Right a)          = pure a

handleAcquireFailure :: Either AcquireFailure a -> IO a
handleAcquireFailure (Left acquireFailure) = error $ "Acquire failure error: " <> show acquireFailure
handleAcquireFailure (Right a)             = pure a

encodeVoteRegistration :: forall era mode. IsShelleyBasedEra era => LocalNodeConnectInfo mode -> ShelleyBasedEra era -> AddressAny -> SlotNo -> Vote -> IO (TxBody era)
encodeVoteRegistration connectInfo era addr ttl vote = do
  chainTip <- getLocalChainTip connectInfo

  let
    consensusMode = consensusModeOnly $ localConsensusModeParams connectInfo
    mEraInMode = toEraInMode cardanoEra consensusMode

  case mEraInMode of
    Nothing        -> error $ "Failed to create EraInMode from: '" <> show era <> "' and '" <> show consensusMode <> "'."
    Just eraInMode -> do
      let
        addrShelley :: AddressInEra era
        addrShelley = anyAddressInShelleyBasedEra addr
      --
      -- Get the network parameters
      pparams <- handleEraMismatch =<< handleAcquireFailure =<< queryNodeLocalState connectInfo Nothing (QueryInEra eraInMode (QueryInShelleyBasedEra era QueryProtocolParameters))

      let
        meta      = voteToTxMetadata vote
        networkId = localNodeNetworkId connectInfo

      -- Estimate the fee for the transaction
      let
        feeParams = estimateVoteFeeParams networkId era pparams meta

      -- Find some unspent funds
      utxos <- fmap fromUtxos $ handleEraMismatch =<< handleAcquireFailure =<< queryNodeLocalState connectInfo Nothing (QueryInEra eraInMode (QueryInShelleyBasedEra era (QueryUTxO (QueryUTxOByAddress $ Set.singleton addr))))
      case findUnspent feeParams utxos of
        Nothing      -> error $ "Not enough funds to meet fee in '" <> show utxos <> "'." -- throwError $ _NotEnoughFundsToMeetFeeError # utxos
        Just unspent -> do
          let
            slotTip          = (\case
                                   (ChainTip slot _ _) -> slot
                                   (ChainTipAtGenesis) -> 0) $ chainTip
            txins            = unspentSources unspent
            (Lovelace value) = unspentValue unspent
            (Lovelace fee)   =
              estimateVoteTxFee
                networkId era pparams slotTip txins addrShelley (Lovelace value) meta

          -- Create the vote transaction
          pure $ voteRegistrationTx era addrShelley txins (Lovelace $ value - fee) (slotTip + ttl) (Lovelace fee) meta

-- | Helper for creating a transaction body.
voteRegistrationTx
  :: IsShelleyBasedEra era
  => ShelleyBasedEra era
  -> AddressInEra era
  -> [TxIn]
  -> Lovelace
  -> SlotNo
  -> Lovelace
  -> TxMetadata
  -> TxBody era
voteRegistrationTx era addr txins (Lovelace value) ttl (Lovelace fee) meta =
 let
   txoutVal =
     case multiAssetSupportedInEra cardanoEra of
       Left adaOnlyInEra -> TxOutAdaOnly adaOnlyInEra (Lovelace value)
       Right multiAssetInEra -> TxOutValue multiAssetInEra (lovelaceToValue (Lovelace value))
   txouts = [TxOut addr txoutVal TxOutDatumNone]

   validityUpperBound = case era of
    ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra ttl
    ShelleyBasedEraAllegra -> TxValidityUpperBound ValidityUpperBoundInAllegraEra ttl
    ShelleyBasedEraMary    -> TxValidityUpperBound ValidityUpperBoundInMaryEra ttl
    ShelleyBasedEraAlonzo  -> TxValidityUpperBound ValidityUpperBoundInAlonzoEra ttl
 in
   either (error . show) id $ makeTransactionBody $
       TxBodyContent
           ((, BuildTxWith $ KeyWitness KeyWitnessForSpending) <$> txins)
           TxInsCollateralNone
           txouts
           (liftShelleyBasedTxFee era $ Lovelace fee)
           (TxValidityNoLowerBound, validityUpperBound)
           (liftShelleyBasedMetadata era meta)
           TxAuxScriptsNone
           TxExtraKeyWitnessesNone
           (BuildTxWith Nothing)
           TxWithdrawalsNone
           TxCertificatesNone
           TxUpdateProposalNone
           TxMintNone
           TxScriptValidityNone

-- | Sign a transaction body to create a transaction.
signTx
  :: forall era
   . IsShelleyBasedEra era
  => VotePaymentKey
  -> TxBody era
  -> Tx era
signTx psk txbody =
  let
    witness = withWitnessPaymentKey psk $ makeShelleyKeyWitness txbody
  in
    makeSignedTransaction [witness] txbody

-- | Pretty print a transaction.
prettyTx :: IsCardanoEra era => Tx era -> String
prettyTx = BSC.unpack . textEnvelopeToJSON Nothing

-- | Pretty print a transaction.
prettyTxBody :: IsCardanoEra era => TxBody era -> String
prettyTxBody = BSC.unpack . textEnvelopeToJSON Nothing
