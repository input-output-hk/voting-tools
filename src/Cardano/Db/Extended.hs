{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Db.Extended where

import           Cardano.Db

deriving instance Show TxMetadata
deriving instance Show Tx
deriving instance Show Block
deriving instance Show SlotLeader
deriving instance Show TxOut
deriving instance Show StakeAddress

deriving instance Eq TxMetadata
deriving instance Eq Tx
deriving instance Eq Block
deriving instance Eq SlotLeader
deriving instance Eq TxOut
deriving instance Eq StakeAddress

deriving instance Ord TxMetadata
deriving instance Ord Tx
deriving instance Ord Block
deriving instance Ord SlotLeader
deriving instance Ord TxOut
deriving instance Ord StakeAddress
deriving instance Ord DbWord64
