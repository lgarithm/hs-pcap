module Net.Icmp.Format where

import           Data.Word (Word16, Word32, Word8)

data IcmpHeader = IcmpHeader { icmpType       :: Word8
                             , icmpCode       :: Word8
                             , icmpChecksum   :: Word16
                             , icmpRestHeader :: Word32
                             }

data IcmpPacket = IcmpPacket { icmpHeader :: IcmpHeader
                             , icmpData   :: [Word8]
                             }
