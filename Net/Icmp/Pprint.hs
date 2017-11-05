{-# LANGUAGE StandaloneDeriving #-}
module Net.Icmp.Pprint where
import           Net.Icmp.Format

deriving instance Show IcmpHeader
deriving instance Show IcmpPacket
