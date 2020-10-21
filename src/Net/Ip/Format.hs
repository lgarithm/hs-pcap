module Net.Ip.Format where

import           Data.Bits (shiftR, (.&.))
import           Data.Word (Word16, Word32, Word8)

data IpFlags = DF | MF deriving (Eq, Ord, Enum)

newtype IPv4Addr = IPv4Addr Word32

data IpHeader = Ipv4Header { _versionAndIhl    :: Word8  -- 4, 4
                           , _dscpAndECN       :: Word8  -- 6, 2
                           , ipPacketTotLen    :: Word16
                           , identification    :: Word16
                           , _ipFlagsAndOffset :: Word16  -- 3, 13
                           , ttl               :: Word8
                           , protocol          :: Word8
                           , ipCheckSum        :: Word16
                           , ipSrcAddr         :: IPv4Addr
                           , ipDstAddr         :: IPv4Addr
                           }

data IpPacket = IpPacket { ipHeader :: IpHeader
                         , ipData   :: [Word8]
                         }

version = (`shiftR` 4) . _versionAndIhl
ihl = (.&. 0x0f) . _versionAndIhl

dscp = (`shiftR` 2) . _dscpAndECN
ecn = (.&. 0x03) . _dscpAndECN

ipFlags = (`shiftR` 13) . _ipFlagsAndOffset
offset = (.&. 0x1fff) . _ipFlagsAndOffset
