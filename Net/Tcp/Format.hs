module Net.Tcp.Format where

import           Data.Bits (shiftR)
import           Data.Word (Word16, Word32, Word8)

newtype TcpOffset = TcpOffset Word8 deriving (Eq, Ord)

data TcpFlag = NS
             | CWR
             | ECE
             | URG
             | ACK
             | PUH
             | RST
             | SYN
             | FIN
             deriving (Eq, Enum, Ord)

tcpFlagMask = shiftR 0x0100 . fromEnum :: TcpFlag -> Word16

data TcpHeader = TcpHeader { tcpSrcPort    :: Word16
                           , tcpDstPort    :: Word16
                           , seq           :: Word32
                           , ack           :: Word32
                           , offset        :: TcpOffset
                           , flags         :: [TcpFlag]
                           , tcpWindowSize :: Word16
                           , tcpIpCheckSum :: Word16
                           , urgPointer    :: Word16
                           }

newtype TcpOptionalHeader = TcpOptionalHeader [Word8]

data TcpPacket = TcpPacket { tcpHeader         :: TcpHeader
                           , tcpOptionalHeader :: Maybe TcpOptionalHeader
                           , tcpData           :: [Word8]
                           }
