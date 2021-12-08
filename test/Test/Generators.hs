module Test.Generators where


import           Control.Monad.Except
import           Data.Maybe (fromMaybe)
import           Data.Word
import           Hedgehog (Gen, MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Api (AsType (AsStakeExtendedKey, AsStakeKey), Lovelace,
                   NetworkId (Mainnet, Testnet), NetworkMagic (..), StakeAddress,
                   TxMetadata (TxMetadata),
                   TxMetadataValue (TxMetaBytes, TxMetaList, TxMetaMap, TxMetaNumber, TxMetaText),
                   deserialiseFromRawBytes, generateSigningKey, getVerificationKey,
                   verificationKeyHash)

import           Cardano.API.Extended (AsType (AsVotingKeyPublic), VotingKeyPublic)
import           Cardano.CLI.Voting
import           Cardano.CLI.Voting.Metadata (Vote, VotePayload, mkVotePayload)
import           Cardano.CLI.Voting.Signing (VoteSigningKey, VoteVerificationKey,
                   getVoteVerificationKey, toStakeAddr, voteSigningKeyFromStakeExtendedSigningKey,
                   voteSigningKeyFromStakeSigningKey)
import           Contribution (Contributions)
import qualified Contribution as Contrib

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

-- | Generate random contributions.
--
-- Word8 was chosen because it is large enough to give us a decent
-- range of values, but small enough that generating random Word8's is
-- fairly likely to result in duplicate values, which are exactly the
-- values we are interested in testing. You are also more likely to
-- encounter overflow errors with such a small maximum bound.
contributions :: Gen (Contributions Word8 Word8 Word8)
contributions = Gen.recursive Gen.choice
  [ mempty ]
  [ Contrib.contribute <$> Gen.word8 (Range.linear 0 maxBound) <*> Gen.word8 (Range.linear 0 maxBound) <*> Gen.word8 (Range.linear 0 maxBound) <*> contributions
  , Contrib.withdraw <$> Gen.word8 (Range.linear 0 maxBound) <*> Gen.word8 (Range.linear 0 maxBound) <*> contributions
  , (<>) <$> contributions <*> contributions
  ]

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

votingKeyPublic :: MonadGen m => m VotingKeyPublic
votingKeyPublic =
  fromMaybe (error "Deserialising VotingKeyPublic from bytes failed!")
  <$> deserialiseFromRawBytes AsVotingKeyPublic
  <$> Gen.bytes (Range.linear 0 128)

voteSigningKey :: (MonadGen m, MonadIO m) => m VoteSigningKey
voteSigningKey = do
  a <- liftIO $ voteSigningKeyFromStakeSigningKey <$> generateSigningKey AsStakeKey
  b <- liftIO $ voteSigningKeyFromStakeExtendedSigningKey <$> generateSigningKey AsStakeExtendedKey
  Gen.choice [ pure a
             , pure b
             ]

voteVerificationKey :: (MonadGen m, MonadIO m) => m VoteVerificationKey
voteVerificationKey =
  getVoteVerificationKey <$> voteSigningKey

rewardsAddress :: (MonadGen m, MonadIO m) => m StakeAddress
rewardsAddress = do
  signingKey <- liftIO $ generateSigningKey AsStakeKey
  let hashStakeKey = verificationKeyHash . getVerificationKey $ signingKey

  toStakeAddr
    <$> Gen.choice [ Testnet . NetworkMagic <$> Gen.word32 (Range.linear minBound maxBound), pure Mainnet ]
    <*> pure hashStakeKey

slotNo :: MonadGen m => m Integer
slotNo = fromIntegral <$> Gen.word64 Range.constantBounded

votePayload :: (MonadGen m, MonadIO m) => m VotePayload
votePayload =
  mkVotePayload <$> votingKeyPublic <*> voteVerificationKey <*> rewardsAddress <*> slotNo

vote :: (MonadGen m, MonadIO m) => m Vote
vote =
  createVoteRegistration <$> voteSigningKey <*> votingKeyPublic <*> rewardsAddress <*> slotNo
