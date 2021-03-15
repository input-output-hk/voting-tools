{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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

import           Cardano.Api (Address, AddressAny, AddressInEra (AddressInEra),
                     AsType (AsPaymentKey, AsStakeKey), CardanoEra (..), ChainPoint, ChainTip (..),
                     IsCardanoEra, IsShelleyBasedEra, Key, Lovelace, MaryEra, NetworkId,
                     PaymentCredential, PaymentKey,
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
                     lovelaceToValue, makeShelleyAddress, makeShelleyKeyWitness,
                     makeSignedTransaction, makeTransactionBody, makeTransactionMetadata,
                     multiAssetSupportedInEra, serialiseToBech32, serialiseToRawBytes,
                     serialiseToRawBytesHex, shelleyBasedEra, valueToLovelace, verificationKeyHash)
import           Cardano.Api.Block (ChainPoint (ChainPoint), toConsensusPoint)
import           Cardano.Api.IPC (LocalNodeConnectInfo (LocalNodeConnectInfo), QueryInEra (..),
                     QueryInMode (..),
                     QueryInShelleyBasedEra (QueryChainPoint, QueryProtocolParameters, QueryUTxO),
                     consensusModeOnly, getLocalChainTip, localConsensusModeParams,
                     localNodeNetworkId, queryNodeLocalState)
import           Cardano.Api.LocalChainSync (getLocalTip)
import           Cardano.Api.Modes (ConsensusModeParams (..), EraInMode (..), toEraInMode)
import           Cardano.Api.Shelley (fromMaryValue)
import           Cardano.Api.Typed (AsType (AsSigningKey), Lovelace (Lovelace),
                     PaymentCredential (PaymentCredentialByKey), Shelley, ShelleyLedgerEra,
                     ShelleyMode, ShelleyWitnessSigningKey (WitnessPaymentKey),
                     SigningKey (StakeSigningKey), StakeAddressReference (StakeAddressByValue),
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
import qualified Cardano.Ledger.Shelley.Constraints as Ledger
import qualified Codec.Binary.Bech32 as Bech32
import           Ouroboros.Consensus.Cardano.Block (EraMismatch, Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx)
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import           Ouroboros.Network.Block (Point, getTipSlotNo, pointSlot)
import           Ouroboros.Network.Point (fromWithOrigin)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure, ShowQuery)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)
import qualified Shelley.Spec.Ledger.Coin as Ledger
import qualified Shelley.Spec.Ledger.Hashing as Ledger
import qualified Shelley.Spec.Ledger.Keys as Shelley
import           Shelley.Spec.Ledger.PParams (PParams)
import qualified Shelley.Spec.Ledger.PParams as Shelley
import qualified Shelley.Spec.Ledger.Tx as Ledger
import qualified Shelley.Spec.Ledger.UTxO as Ledger

import           Cardano.API.Extended (AsBech32DecodeError, AsBech32HumanReadablePartError,
                     AsType (AsVotingKeyPublic), VotingKeyPublic, deserialiseFromBech32',
                     liftShelleyBasedEra, liftShelleyBasedMetadata, liftShelleyBasedTxFee,
                     textEnvelopeToJSON)
import           Cardano.CLI.Voting.Error
import           Cardano.CLI.Voting.Fee
import           Cardano.CLI.Voting.Metadata (RewardsAddress, Vote, VotePayload, mkVotePayload,
                     signVotePayload, voteToTxMetadata)
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
  -> RewardsAddress
  -> Vote
createVoteRegistration skey votepub rewardsAddr =
    let
      payload     = mkVotePayload votepub (getVoteVerificationKey skey) rewardsAddr
      payloadCBOR = CBOR.serialize' payload

      payloadSig  :: SigDSIGN (DSIGN StandardCrypto)
      payloadSig  = payloadCBOR `sign` skey
  in
    fromMaybe (error "Failed to sign vote payload") $
      signVotePayload payload payloadSig

-- | Encode the vote registration payload as a transaction body.
-- encodeVoteRegistration
--   :: ( MonadIO m
--      , MonadError e m
--      , AsShelleyQueryCmdLocalStateQueryError e
--      , AsNotEnoughFundsError e

--      , Consensus.ShelleyBasedEra ledgerera
--      , ShelleyLedgerEra era ~ ledgerera
--      , IsShelleyBasedEra era

--      , Ledger.Crypto ledgerera ~ StandardCrypto

--      , ShowProxy block
--      , ShowProxy (ApplyTxErr block)
--      , ShowProxy (Query block)
--      , ShowProxy (GenTx block)
--      , ShowQuery (Query block)
--      )
--   => LocalNodeConnectInfo mode block
--   -> ShelleyBasedEra era
--   -> AddressAny
--   -> SlotNo
--   -> Vote
--   -> m (TxBody era)

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
    cardanoEra = case era of
      ShelleyBasedEraShelley -> ShelleyEra
      ShelleyBasedEraAllegra -> AllegraEra
      ShelleyBasedEraMary    -> MaryEra
    consensusMode = consensusModeOnly $ localConsensusModeParams connectInfo
    mEraInMode = toEraInMode cardanoEra consensusMode

  case mEraInMode of
    Nothing        -> error $ "Failed to create EraInMode from: '" <> show cardanoEra <> "' and '" <> show consensusMode <> "'."
    Just eraInMode -> do
      let
        addrShelley :: IsShelleyBasedEra era => AddressInEra era
        addrShelley = anyAddressInShelleyBasedEra addr
      --
      -- Get the network parameters
      pparams <- handleEraMismatch =<< handleAcquireFailure =<< queryNodeLocalState connectInfo chainTip (QueryInEra eraInMode (QueryInShelleyBasedEra era QueryProtocolParameters))

      let
        meta      = voteToTxMetadata vote
        networkId = localNodeNetworkId connectInfo

      -- Estimate the fee for the transaction
      let
        feeParams = estimateVoteFeeParams networkId era pparams meta

      -- Find some unspent funds
      utxos <- fmap fromUtxos $ handleEraMismatch =<< handleAcquireFailure =<< queryNodeLocalState connectInfo chainTip (QueryInEra eraInMode (QueryInShelleyBasedEra era (QueryUTxO (Just $ Set.singleton addr))))
      case findUnspent feeParams utxos of
        Nothing      -> error $ "Not enough funds to meet fee in '" <> show utxos <> "'." -- throwError $ _NotEnoughFundsToMeetFeeError # utxos
        Just unspent -> do
          liftIO $ print unspent
          let
            slotTip          = (\(ChainPoint slot _) -> slot) $ chainTip
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
