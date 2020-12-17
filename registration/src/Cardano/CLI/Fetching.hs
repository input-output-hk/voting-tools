module Cardano.CLI.Fetching where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BC
import           Data.Int (Int64)
import Data.Scientific (toBoundedInteger)
import qualified Data.ByteString.Base16 as Base16
import           Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import           qualified Data.Aeson as Aeson
import           qualified Data.Aeson.Types as Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import           Cardano.API (Lovelace(Lovelace))
import qualified Cardano.API as Api

import           Cardano.API.Extended (AsBech32DecodeError (_Bech32DecodeError),
                     AsFileError (_FileIOError, __FileError),
                     AsInputDecodeError (_InputDecodeError), AsType (AsVotingKeyPublic),
                     VotingKeyPublic, deserialiseFromBech32', pNetworkId, parseAddress,
                     readSigningKeyFile, readerFromAttoParser, JormungandrAddress)

type Threshold = Int

data VotingFunds
  = VotingFunds { _votingFunds :: Map JormungandrAddress Lovelace }
  deriving (Eq, Show)

instance Semigroup VotingFunds where
  (VotingFunds a) <> (VotingFunds b) = VotingFunds (a <> b)

instance Monoid VotingFunds where
  mempty = VotingFunds mempty

instance FromJSON VotingFunds where
    parseJSON = Aeson.withObject "VotingFunds" $ \v -> do
      let
        decodeLovelace = Aeson.withScientific "Lovelace" $ \n ->
          case toBoundedInteger n of
            Nothing  -> fail $ "failed to convert " <> show n <> " into a valid integer."
            Just num -> pure $ Lovelace $ toInteger (num :: Int64)
  
        kvs = HM.toList v

      kvs' <- traverse (\(k, v) -> (,) <$> (Aeson.parseJSON . Aeson.String $ k) <*> decodeLovelace v) kvs
      pure . VotingFunds . M.fromList $ kvs'

instance ToJSON VotingFunds where
  toJSON (VotingFunds map) =
    let 
      kvs = M.toList map

      toJSONLovelace (Lovelace n) = Aeson.Number $ fromInteger n

      kvs' = (\(k,v) -> (TL.toStrict . TL.decodeUtf8 . Aeson.encode $ k, toJSONLovelace v)) <$> kvs
    in
      Aeson.Object $ HM.fromList kvs'

