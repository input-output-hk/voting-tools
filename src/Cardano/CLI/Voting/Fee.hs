{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Tools used to estimate the fee associated with a vote transaction
-- and ensure that the unspent value of the TxIns are enough to cover
-- the fee associated with submitting the transaction.
--
-- I use two heuristics to calculate this:
--   1. A base fee estimate, calculated by estimating the fee of a
--      vote transaction with no TxIns.
--   2. A per-txin fee estimate. If the first UTxO we find cannot
--      cover the cost of the fee, we need to find additional UTxOs.
--      Each of these UTxOs forms a TxIn which provides funds for our
--      vote transaction. Increasing the number of TxIns in a
--      transaction increases the fee associated with that
--      transaction. In the pathological case, each extra TxIn may
--      introduce more fees than funds. We use the per-txin fee
--      estimate to ensure we fail appropriately in this case.
--
-- These two parameters are encoded in the "FeeParams" type.

module Cardano.CLI.Voting.Fee where

import           Data.String (fromString)

import           Cardano.API.Extended (liftShelleyBasedMetadata, liftShelleyBasedTxFee)
import           Cardano.Api (AddressInEra, AsType (AsPaymentKey, AsStakeKey), BuildTxWith (..),
                   IsShelleyBasedEra (..), Key, KeyWitnessInCtx (..), Lovelace (..), NetworkId,
                   PaymentCredential (..), PaymentKey, ShelleyBasedEra (..), SigningKey, SlotNo,
                   StakeAddressReference (..), StakeCredential, StakeKey,
                   TxAuxScripts (TxAuxScriptsNone), TxBodyContent (TxBodyContent),
                   TxCertificates (TxCertificatesNone), TxExtraKeyWitnesses (..), TxId (..),
                   TxIn (TxIn), TxInsCollateral (..), TxIx (..), TxMetadata,
                   TxMintValue (TxMintNone), TxOut (..), TxOutDatum (..),
                   TxOutValue (TxOutAdaOnly, TxOutValue), TxScriptValidity (..),
                   TxUpdateProposal (TxUpdateProposalNone), TxValidityLowerBound (..),
                   TxValidityUpperBound (..), TxWithdrawals (TxWithdrawalsNone), UTxO (..),
                   ValidityUpperBoundSupportedInEra (..), VerificationKey, Witness (..), cardanoEra,
                   deterministicSigningKey, deterministicSigningKeySeedSize, estimateTransactionFee,
                   getVerificationKey, lovelaceToValue, makeShelleyAddressInEra,
                   makeSignedTransaction, makeTransactionBody, multiAssetSupportedInEra,
                   selectLovelace, verificationKeyHash)
import           Cardano.Api.Shelley (ProtocolParameters (..), StakeCredential (..))
import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Seed as Crypto
import           Control.Lens.TH (makeClassyPrisms)
import qualified Data.Map.Strict as M

-- | Fee characteristics of a transaction
data FeeParams = FeeParams
    { feeBase    :: Lovelace
    -- ^ The fee of a vote transaction with no TxIns
    , feePerTxIn :: Lovelace
    -- ^ The increase in fees for adding a TxIn
    }
  deriving (Eq, Show)

-- | Transactions that aren't fully spent.
type UnspentSources
  = [( TxIn
     -- ^ Transaction in
     , Lovelace
     -- ^ Unspent amount
     )]

data NotEnoughFundsError = NotEnoughFundsToMeetFeeError !UnspentSources
  deriving (Eq, Show)

makeClassyPrisms ''NotEnoughFundsError

fromUtxos :: UTxO era -> UnspentSources
fromUtxos (UTxO m) = foldr
  (\(txIn, TxOut _ val _) -> case val of
    (TxOutAdaOnly _ l) -> ((txIn, l):)
    (TxOutValue _ v)   -> if selectLovelace v == Lovelace 0 then id else ((txIn, selectLovelace v):)
  )
  [] (M.assocs m)

-- | Total amount of unspent value.
unspentValue :: UnspentSources -> Lovelace
unspentValue =
  foldr
    (\(_, Lovelace unspent) (Lovelace acc) -> Lovelace $ acc + unspent)
    (Lovelace 0)

-- | All transactions that aren't fully spent.
unspentSources :: UnspentSources -> [TxIn]
unspentSources = foldr (\(txin, _) -> (txin:)) mempty

-- | Estimate the fee required for a vote transaction.
estimateVoteTxFee
  :: IsShelleyBasedEra era
  => NetworkId
  -> ShelleyBasedEra era
  -> ProtocolParameters
  -> SlotNo
  -> [TxIn]
  -> AddressInEra era
  -> Lovelace
  -> TxMetadata
  -> Lovelace
estimateVoteTxFee networkId era pparams ttl txins addr txBaseValue meta =
  let
    txBaseValue' =
      case multiAssetSupportedInEra cardanoEra of
        Left adaOnlyInEra -> TxOutAdaOnly adaOnlyInEra txBaseValue
        Right multiAssetInEra -> TxOutValue multiAssetInEra (lovelaceToValue txBaseValue)

    -- txoutsNoFee :: [TxOut ctx era]
    txoutsNoFee = [TxOut addr txBaseValue' TxOutDatumNone]

    validityUpperBound = case era of
      ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra ttl
      ShelleyBasedEraAllegra -> TxValidityUpperBound ValidityUpperBoundInAllegraEra ttl
      ShelleyBasedEraMary    -> TxValidityUpperBound ValidityUpperBoundInMaryEra ttl
      ShelleyBasedEraAlonzo  -> TxValidityUpperBound ValidityUpperBoundInAlonzoEra ttl


    txBody = either (error . show) id $ makeTransactionBody
               (TxBodyContent
                 ((, BuildTxWith $ KeyWitness KeyWitnessForSpending) <$> txins)
                 TxInsCollateralNone
                 txoutsNoFee
                 (liftShelleyBasedTxFee era $ fromInteger 0)
                 (TxValidityNoLowerBound, validityUpperBound)
                 (liftShelleyBasedMetadata era meta)
                 TxAuxScriptsNone
                 TxExtraKeyWitnessesNone
                 (BuildTxWith $ Just pparams)
                 TxWithdrawalsNone
                 TxCertificatesNone
                 TxUpdateProposalNone
                 TxMintNone
                 TxScriptValidityNone
               )
    tx = makeSignedTransaction [] txBody
  in
    estimateTransactionFee
      networkId
      (protocolParamTxFeeFixed pparams)
      (protocolParamTxFeePerByte pparams)
      tx
      (length txins)
      (length txoutsNoFee)
      1
      0

-- | Estimate the fee characteristics of a vote transaction. The base
-- fee, and the how the fee changes as we add more transaction inputs.
estimateVoteFeeParams
  :: IsShelleyBasedEra era
  => NetworkId
  -> ShelleyBasedEra era
  -> ProtocolParameters
  -> TxMetadata
  -> FeeParams
estimateVoteFeeParams networkId era pparams meta =
  let
    mockTTL :: SlotNo
    mockTTL = 1

    mockSkey :: Key keyrole => AsType keyrole -> SigningKey keyrole
    mockSkey keyrole = deterministicSigningKey keyrole (Crypto.mkSeedFromBytes (fromString $ replicate (fromInteger $ toInteger $ deterministicSigningKeySeedSize keyrole) 'x'))

    mockVkey :: VerificationKey PaymentKey
    mockVkey = getVerificationKey (mockSkey AsPaymentKey)

    mockPaymentCredential :: PaymentCredential
    mockPaymentCredential = PaymentCredentialByKey $ verificationKeyHash mockVkey

    mockVkeyStake :: VerificationKey StakeKey
    mockVkeyStake = getVerificationKey (mockSkey AsStakeKey)

    mockStakeCredential :: StakeCredential
    mockStakeCredential = StakeCredentialByKey $ verificationKeyHash mockVkeyStake

    mockStakeAddressRef :: StakeAddressReference
    mockStakeAddressRef = StakeAddressByValue mockStakeCredential

    mockAddr :: IsShelleyBasedEra era => AddressInEra era
    mockAddr = makeShelleyAddressInEra networkId mockPaymentCredential mockStakeAddressRef

    -- Start with a base estimate
    -- Estimate the fee per extra txin
    mockTxInA   = TxIn (TxId $ Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()) (TxIx 1)
    mockTxInB   = TxIn (TxId $ Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()) (TxIx 1)
    (Lovelace feeBase')    = estimateVoteTxFee networkId era pparams mockTTL [mockTxInA] mockAddr (Lovelace 0) meta
    (Lovelace feeWithMockTxIn) = estimateVoteTxFee networkId era pparams mockTTL [mockTxInA,  mockTxInB] mockAddr (Lovelace 0) meta
    feePerTxIn' = Lovelace $ feeWithMockTxIn - feeBase'
  in
    FeeParams (Lovelace feeBase') feePerTxIn'

findUnspent
  :: FeeParams
  -- ^ Estimates for the base fee and the fee per TxIn
  -> UnspentSources
  -- ^ UTxOs we can use to cover fees
  -> Maybe UnspentSources
  -- ^ Nothing if given UTxOs cannot cover fees. Otherwise, UTxOs
  -- actually used to cover fees.
findUnspent feeParams utxos =
  case takeUntilFeePaid feeParams utxos of
    [] -> Nothing
    xs -> Just xs

takeUntilFeePaid :: FeeParams -> [(a , Lovelace)] -> [(a , Lovelace)]
takeUntilFeePaid (FeeParams feeBase' (Lovelace feePerTxIn')) =
  let
    initialFeeTarget = feeBase'
  in
    go initialFeeTarget (Lovelace 0)

  where
    go :: Lovelace -> Lovelace -> [(a, Lovelace)] -> [(a, Lovelace)]
    go (Lovelace target) (Lovelace acc) xs
      | acc >= target = xs
    go _ _ []
      = []
    go (Lovelace target) (Lovelace acc) ((a, Lovelace unspent):xs)
      =
      let
        newTarget = Lovelace $ target + feePerTxIn'
        newAcc    = Lovelace $ acc + unspent
        txs       = go newTarget newAcc xs
      in
        (a, Lovelace unspent):txs
