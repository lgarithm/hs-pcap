module Net.Udp.Parse where

import           Misc.Binary                   (FromBytes (..))
import           Misc.Parse                    (anyByte, decodeBytesWith,
                                                word16)
import           Net.Udp.Format                (UdpHeader (UdpHeader),
                                                UdpPacket (UdpPacket))
import           Text.Parsec.ByteString        (Parser)
import           Text.ParserCombinators.Parsec (many)

pUdpHeader = do { sp <- word16
                ; dp <- word16
                ; len <- word16
                ; sum <- word16
                ; return $ UdpHeader sp dp len sum
                } :: Parser UdpHeader

pUdpPacket = do { uh <- pUdpHeader
                ; ud <- many anyByte
                ; return $ UdpPacket uh ud
                } :: Parser UdpPacket

instance FromBytes UdpHeader where
  decode = decodeBytesWith pUdpHeader

instance FromBytes UdpPacket where
  decode = decodeBytesWith pUdpPacket
