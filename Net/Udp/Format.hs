module Net.Udp.Format where

import           Data.Word (Word16, Word8)

data UdpHeader = UdpHeader { udpSrcPort  :: Word16
                           , udpDstPort  :: Word16
                           , udpLength   :: Word16
                           , udpChecksum :: Word16
                           }

data UdpPacket = UdpPacket { udpHeader :: UdpHeader
                           , udpData   :: [Word8]
                           }
