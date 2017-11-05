{-# LANGUAGE StandaloneDeriving #-}

module Net.Udp.Pprint where

import           Net.Udp.Format (UdpHeader (UdpHeader), UdpPacket (UdpPacket))

deriving instance Show UdpHeader
deriving instance Show UdpPacket
