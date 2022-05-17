
module Cardano.Catalyst.Query.Types where

import qualified Cardano.Api as Api
import qualified Data.Aeson as Aeson

-- | The Query datatype represents the interface to a data source that can
-- provide the information necessary to run the voting-tools application.
--
-- In this case 'm' refers to some monadic context, and 't' refers to a
-- "time-like" value that can be used to order vote registrations according to
-- time of registration.
data Query m t =
  Query { queryVoteRegistrations
          :: Maybe Api.SlotNo -> m [(t, Aeson.Value)]
        -- ^ Get the vote registrations made at and before the slot number.
        , queryStakeValue
          :: Maybe Api.SlotNo
          -> Api.StakeAddress
          -> m Integer
        -- ^ Get the ADA associated with a stake address at a slot number.
        , queryStakeValues
          :: Maybe Api.SlotNo
          -> [Api.StakeAddress]
          -> m [(Api.StakeAddress, Integer)]
        -- ^ Get the ADA associated with a list of stake addresses at a slot
        -- number. The plural of 'queryStakeValue'.
        --
        -- ∀xs. length xs === length (queryStakeValues xs)
        -- ∀xs. queryStakeValues xs === zip stakeAddrs <$> traverse queryStakeValue xs
        }
