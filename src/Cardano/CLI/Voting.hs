{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Performs the bulk of the work creating the vote registration
-- transaction.

module Cardano.CLI.Voting where

import           Control.Lens (( # ))
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Text (Text)

import           Cardano.API (Address, AddressAny, AddressInEra (AddressInEra),
                     AsType (AsPaymentKey, AsStakeKey), IsCardanoEra, IsShelleyBasedEra, Key,
                     LocalNodeConnectInfo, Lovelace, MaryEra, NetworkId, PaymentCredential,
                     PaymentKey,
                     ShelleyBasedEra (ShelleyBasedEraAllegra, ShelleyBasedEraMary, ShelleyBasedEraShelley),
                     ShelleyEra, SigningKey, SlotNo, StakeAddressReference, StakeCredential,
                     StakeKey, Tx, TxAuxScripts (TxAuxScriptsNone), TxBody,
                     TxBodyContent (TxBodyContent), TxCertificates (TxCertificatesNone),
                     TxIn (TxIn), TxMetadata, TxMintValue (TxMintNone), TxOutValue (TxOutAdaOnly),
                     TxOutValue (TxOutValue), TxUpdateProposal (TxUpdateProposalNone),
                     TxValidityLowerBound (TxValidityNoLowerBound),
                     TxValidityUpperBound (TxValidityUpperBound),
                     TxWithdrawals (TxWithdrawalsNone),
                     ValidityUpperBoundSupportedInEra (ValidityUpperBoundInAllegraEra, ValidityUpperBoundInMaryEra, ValidityUpperBoundInShelleyEra),
                     VerificationKey, anyAddressInShelleyBasedEra, castVerificationKey,
                     deserialiseFromRawBytesHex, estimateTransactionFee, getVerificationKey,
                     localNodeNetworkId, lovelaceToValue, makeShelleyAddress,
                     makeShelleyKeyWitness, makeSignedTransaction, makeTransactionBody,
                     makeTransactionMetadata, multiAssetSupportedInEra, serialiseToBech32,
                     serialiseToRawBytes, serialiseToRawBytesHex, valueToLovelace,
                     verificationKeyHash)
import           Cardano.Api.LocalChainSync (getLocalTip)
import           Cardano.Api.Shelley (fromMaryValue)
import           Cardano.Api.Typed (AsType (AsSigningKey), Lovelace (Lovelace),
                     PaymentCredential (PaymentCredentialByKey), Shelley, ShelleyLedgerEra,
                     ShelleyWitnessSigningKey (WitnessPaymentKey), SigningKey (StakeSigningKey),
                     StakeAddressReference (StakeAddressByValue),
                     StakeCredential (StakeCredentialByKey), StandardAllegra, StandardShelley,
                     TxId (TxId), TxIx (TxIx),
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
import           Cardano.Ledger.Crypto (Crypto (..))
import qualified Cardano.Ledger.Era as Ledger
import           Cardano.Ledger.Mary.Value
import           Cardano.Ledger.Shelley as Shel
import qualified Codec.Binary.Bech32 as Bech32
import           Ouroboros.Consensus.Cardano.Block (Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx)
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import           Ouroboros.Network.Block (getTipSlotNo)
import           Ouroboros.Network.Point (fromWithOrigin)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (ShowQuery)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)
import qualified Shelley.Spec.Ledger.Coin as Ledger
import qualified Shelley.Spec.Ledger.Hashing as Ledger
import qualified Shelley.Spec.Ledger.Keys as Shelley
import           Shelley.Spec.Ledger.PParams (PParams)
import qualified Shelley.Spec.Ledger.PParams as Shelley
import qualified Shelley.Spec.Ledger.Tx as Ledger
import qualified Shelley.Spec.Ledger.UTxO as Ledger

import           Cardano.API.Extended (liftShelleyBasedEra, liftShelleyBasedMetadata,
                     liftShelleyBasedTxFee, textEnvelopeToJSON)
import           Cardano.API.Extended (AsBech32DecodeError, AsBech32HumanReadablePartError,
                     AsShelleyQueryCmdLocalStateQueryError, AsType (AsVotingKeyPublic),
                     VotingKeyPublic, deserialiseFromBech32', queryPParamsFromLocalState,
                     queryUTxOFromLocalState)
import           Cardano.CLI.Voting.Error
import           Cardano.CLI.Voting.Fee
import           Cardano.CLI.Voting.Metadata (Vote, VotePayload, mkVotePayload, signVotePayload,
                     voteToTxMetadata)
import           Cardano.CLI.Voting.Signing (VotePaymentKey, VoteSigningKey,
                     withVoteShelleySigningKey, withVoteSigningKey, withWitnessPaymentKey)
import           Cardano.CLI.Voting.Signing (getVoteVerificationKey, sign, verify)

import qualified Cardano.Ledger.Compactible
import qualified Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley
import qualified Cardano.Ledger.Shelley.Constraints
import qualified Cardano.Ledger.ShelleyMA.TxBody
import qualified Cardano.Ledger.Torsor
import qualified Cardano.Ledger.Val
import qualified NoThunks.Class
import qualified Shelley.Spec.Ledger.API
import qualified Shelley.Spec.Ledger.Coin
import qualified Shelley.Spec.Ledger.Hashing
import qualified Shelley.Spec.Ledger.TxBody
import qualified Shelley.Spec.Ledger.UTxO

-- | Create a vote registration payload.
createVoteRegistration
  :: VoteSigningKey
  -> VotingKeyPublic
  -> Vote
createVoteRegistration skey votepub =
    let
      payload     = mkVotePayload votepub (getVoteVerificationKey skey)
      payloadCBOR = CBOR.serialize' payload

      payloadSig  :: SigDSIGN (DSIGN StandardCrypto)
      payloadSig  = payloadCBOR `sign` skey
  in
    fromMaybe (error "Failed to sign vote payload") $
      signVotePayload payload payloadSig

-- | Encode the vote registration payload as a transaction body.
encodeVoteRegistration
  :: ( MonadIO m
     , MonadError e m
     , AsShelleyQueryCmdLocalStateQueryError e
     , AsNotEnoughFundsError e

     , Consensus.ShelleyBasedEra ledgerera
     , ShelleyLedgerEra era ~ ledgerera
     , IsShelleyBasedEra era

     , Ledger.Crypto ledgerera ~ StandardCrypto
     , Ledger.HashIndex (Cardano.Ledger.Core.TxBody ledgerera) ~ Ledger.EraIndependentTxBody
     , Ledger.Era ledgerera
     , Cardano.Ledger.Shelley.Constraints.ShelleyBased ledgerera


     , ShowProxy block
     , ShowProxy (ApplyTxErr block)
     , ShowProxy (Query block)
     , ShowProxy (GenTx block)
     , ShowQuery (Query block)
     )
  => LocalNodeConnectInfo mode block
  -> ShelleyBasedEra era
  -> AddressAny
  -> SlotNo
  -> Vote
  -> m (TxBody era)
encodeVoteRegistration connectInfo era addr ttl vote = do
  let
    addrShelley :: IsShelleyBasedEra era => AddressInEra era
    addrShelley = anyAddressInShelleyBasedEra addr
  -- Get the network parameters
  pparams <- queryPParamsFromLocalState connectInfo
  let
    meta      = voteToTxMetadata vote
    networkId = localNodeNetworkId connectInfo

  -- Estimate the fee for the transaction
  let
    feeParams = estimateVoteFeeParams networkId era pparams meta

  -- Find some unspent funds
  utxos  <- queryUTxOFromLocalState era (FilterByAddress $ Set.singleton addr) connectInfo
  case findUnspent feeParams (fromShelleyUTxO utxos) of
    Nothing      -> throwError $ _NotEnoughFundsToMeetFeeError # fromShelleyUTxO utxos
    Just unspent -> do
      tip <- liftIO $ getLocalTip connectInfo
      let
        slotTip          = fromWithOrigin minBound $ getTipSlotNo tip
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
     case multiAssetSupportedInEra (liftShelleyBasedEra era) of
       Left adaOnlyInEra -> TxOutAdaOnly adaOnlyInEra (Lovelace value)
       Right multiAssetInEra -> TxOutValue multiAssetInEra (lovelaceToValue (Lovelace value))
   txouts = [TxOut addr txoutVal]

   validityUpperBound = case era of
    ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra ttl
    ShelleyBasedEraAllegra -> TxValidityUpperBound ValidityUpperBoundInAllegraEra ttl
    ShelleyBasedEraMary    -> TxValidityUpperBound ValidityUpperBoundInMaryEra ttl
 in
   either (error . show) id $ makeTransactionBody (TxBodyContent
                         txins
                         txouts
                         (liftShelleyBasedTxFee era $ Lovelace fee)
                         (TxValidityNoLowerBound, validityUpperBound)
                         (liftShelleyBasedMetadata era meta)
                         TxAuxScriptsNone
                         TxWithdrawalsNone
                         TxCertificatesNone
                         TxUpdateProposalNone
                         TxMintNone
                       )

-- | Sign a transaction body to create a transaction.
signTx
  :: IsShelleyBasedEra era
  => VotePaymentKey
  -> TxBody era
  -> Tx era
signTx psk txbody =
  let
    witness = withWitnessPaymentKey psk $ makeShelleyKeyWitness txbody
  in
    makeSignedTransaction [witness] txbody

-- | Pretty print a transaction.
prettyTx :: (IsCardanoEra era) => Tx era -> String
prettyTx = BSC.unpack . textEnvelopeToJSON Nothing

fromShelleyUTxO
  :: ( Ledger.Crypto ledgerera ~ StandardCrypto
     , Ledger.HashIndex (Cardano.Ledger.Core.TxBody ledgerera) ~ Ledger.EraIndependentTxBody
     , Ledger.Era ledgerera
     , Cardano.Ledger.Shelley.Constraints.ShelleyBased ledgerera
     )
  => Shelley.Spec.Ledger.UTxO.UTxO ledgerera
  -> UnspentSources
fromShelleyUTxO = fmap (\(txin, txout) -> (fromShelleyTxIn txin, txOutLovelace txout)) . M.assocs . Ledger.unUTxO

txOutLovelace
  :: ( Cardano.Ledger.Shelley.Constraints.ShelleyBased era
     )
  => Shelley.Spec.Ledger.TxBody.TxOut era -> Lovelace
txOutLovelace (Shelley.Spec.Ledger.TxBody.TxOut _ v) = fromIntegral $ Ledger.unCoin $ Cardano.Ledger.Val.coin v

fromShelleyTxIn
  :: ( Ledger.Era ledgerera
     , Ledger.Crypto ledgerera ~ StandardCrypto
     )
  => Ledger.TxIn ledgerera
  -> TxIn
fromShelleyTxIn (Ledger.TxIn txid txix) =
    TxIn (fromShelleyTxId txid) (TxIx (fromIntegral txix))

fromShelleyTxId
  :: Ledger.Crypto ledgerera ~ StandardCrypto
  => Ledger.TxId ledgerera
  -> TxId
fromShelleyTxId (Ledger.TxId h) =
    TxId (Crypto.castHash h)
