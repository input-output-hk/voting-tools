module Test.Generators where


import           Control.Monad.Except
import           Data.Maybe (fromMaybe)
import           Data.Word
import           Hedgehog (Gen, MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Api (AsType (AsStakeExtendedKey, AsStakeKey), Lovelace,
                   NetworkId (Mainnet, Testnet), NetworkMagic (..), TxMetadata (TxMetadata),
                   TxMetadataValue (TxMetaBytes, TxMetaList, TxMetaMap, TxMetaNumber, TxMetaText),
                   deserialiseFromRawBytes, generateSigningKey, getVerificationKey,
                   verificationKeyHash)

import           Cardano.API.Extended (AsType (AsVotingKeyPublic), VotingKeyPublic)
import           Cardano.CLI.Voting
import           Cardano.CLI.Voting.Metadata (RewardsAddress (..), Vote, VotePayload, mkVotePayload)
import           Cardano.CLI.Voting.Signing (StakeSigningKey, StakeVerificationKey,
                   getStakeVerificationKey, signingKeyFromStakeExtendedSigningKey,
                   signingKeyFromStakeSigningKey, stakeAddressFromKeyHash)
import           Cardano.Catalyst.Presentation (VotingPower (..))

-- votingFunds :: Gen VotingFunds
-- votingFunds = VotingFunds <$> Gen.map (Range.linear 0 16) ((,) <$> jaddr <*> lovelace)

-- Gen valid Bech32
-- https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#bech32

-- votingKeyPublic :: Gen VotingKeyPublic
-- votingKeyPublic = do
--   bs <- Gen.bytes (Range.linear 0 256)
--   case deserialiseFromRawBytes AsVotingKeyPublic bs of
--     Nothing  -> fail $ "Failed to create VotingKeyPublic from raw bytes: " <> show bs
--     Just key -> pure key

-- jaddr :: Gen JormungandrAddress

lovelace :: Gen Lovelace
lovelace = (fromIntegral . toInteger) <$> Gen.int64 (Range.linear 0 maxBound)

data OrderedPayload = OrderedPayload Int Word8
  deriving (Show)

instance Eq OrderedPayload where
  (OrderedPayload i1 _) == (OrderedPayload i2 _) = i1 == i2

instance Ord OrderedPayload where
  compare (OrderedPayload i1 _) (OrderedPayload i2 _) = compare i1 i2

orderedPayload :: Gen OrderedPayload
orderedPayload = OrderedPayload <$> Gen.int (Range.linear 0 maxBound) <*> Gen.word8 (Range.linear 0 maxBound)

txMetadataKey :: Gen Word64
txMetadataKey = Gen.word64 (Range.linear minBound maxBound)

txMetadataMapKey :: Gen TxMetadataValue
txMetadataMapKey = Gen.choice [ TxMetaNumber <$> Gen.integral (Range.linear (toInteger $ negate (maxBound :: Word64)) (toInteger $ (maxBound :: Word64)))
                              , TxMetaBytes <$> Gen.bytes (Range.linear 0 64)
                              , TxMetaText <$> Gen.text (Range.linear 0 64) Gen.unicodeAll
                              ]

txMetadataValue :: Gen TxMetadataValue
txMetadataValue = Gen.choice [ TxMetaNumber <$> Gen.integral (Range.linear (toInteger $ negate (maxBound :: Word64)) (toInteger $ (maxBound :: Word64)))
                             , TxMetaBytes <$> Gen.bytes (Range.linear 0 64)
                             , TxMetaText <$> Gen.text (Range.linear 0 64) Gen.unicodeAll
                             , TxMetaList <$> Gen.list (Range.linear 0 20) txMetadataValue
                             , TxMetaMap <$> Gen.list (Range.linear 0 20) ((,) <$> txMetadataMapKey <*> txMetadataValue)
                             ]

txMetadata :: Gen TxMetadata
txMetadata = TxMetadata <$> Gen.map (Range.linear 0 20) ((,) <$> txMetadataKey <*> txMetadataValue)

votingPower :: (MonadGen m, MonadIO m) => m VotingPower
votingPower =
  VotingPower
  <$> votingKeyPublic
  <*> stakeVerificationKey
  <*> rewardsAddress
  <*> (fromIntegral <$> Gen.word64 Range.constantBounded)

votingKeyPublic :: MonadGen m => m VotingKeyPublic
votingKeyPublic =
  fromMaybe (error "Deserialising VotingKeyPublic from bytes failed!")
  <$> deserialiseFromRawBytes AsVotingKeyPublic
  <$> Gen.bytes (Range.linear 0 64)
  -- cardano-node enforces that the maximum bytestring length of any metadata is
  -- 64 bytes
  -- (https://github.com/input-output-hk/cardano-node/blob/5cffbcc6b3e2861ed20452f3f6291ee3fe2bf628/cardano-api/src/Cardano/Api/TxMetadata.hs#L190)

voteSigningKey :: (MonadGen m, MonadIO m) => m StakeSigningKey
voteSigningKey = do
  a <- liftIO $ signingKeyFromStakeSigningKey <$> generateSigningKey AsStakeKey
  b <- liftIO $ signingKeyFromStakeExtendedSigningKey <$> generateSigningKey AsStakeExtendedKey
  Gen.choice [ pure a
             , pure b
             ]

stakeVerificationKey :: (MonadGen m, MonadIO m) => m StakeVerificationKey
stakeVerificationKey =
  getStakeVerificationKey <$> voteSigningKey

rewardsAddress :: (MonadGen m, MonadIO m) => m RewardsAddress
rewardsAddress = do
  signingKey <- liftIO $ generateSigningKey AsStakeKey
  let hashStakeKey = verificationKeyHash . getVerificationKey $ signingKey

  network <- Gen.choice [ Testnet . NetworkMagic <$> Gen.word32 (Range.linear minBound maxBound), pure Mainnet ]

  RewardsAddress <$> (stakeAddressFromKeyHash network <$> pure hashStakeKey)

slotNo :: MonadGen m => m Integer
slotNo = fromIntegral <$> Gen.word32 Range.constantBounded

votePayload :: (MonadGen m, MonadIO m) => m VotePayload
votePayload =
  mkVotePayload <$> votingKeyPublic <*> stakeVerificationKey <*> rewardsAddress <*> slotNo

vote :: (MonadGen m, MonadIO m) => m Vote
vote =
  createVoteRegistration <$> voteSigningKey <*> votingKeyPublic <*> rewardsAddress <*> slotNo
