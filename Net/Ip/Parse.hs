module Net.Ip.Parse where

import           Misc.Binary                   (FromBytes (..))
import           Misc.Parse                    (anyByte, decodeBytesWith,
                                                word16, word32)
import           Net.Ip.Format                 (IPv4Addr (IPv4Addr),
                                                IpHeader (Ipv4Header),
                                                IpPacket (IpPacket))
import           Text.Parsec.ByteString        (Parser)
import           Text.ParserCombinators.Parsec (many)

pIPv4Addr = fmap IPv4Addr word32 :: Parser IPv4Addr

pIpv4Header = do { vi <- anyByte
                 ; de <- anyByte
                 ; tl <- word16
                 ; i <- word16
                 ; fo <- word16
                 ; ttl <- anyByte
                 ; p <- anyByte
                 ; cs <- word16
                 ; src <- pIPv4Addr
                 ; desc <- pIPv4Addr
                 ; return $ Ipv4Header vi de tl i fo ttl p cs src desc
                 } :: Parser IpHeader

pIpv4Packet = do { ihv4 <- pIpv4Header
                 ; ipData <- many anyByte
                 ; return $ IpPacket ihv4 ipData
                 } :: Parser IpPacket

instance FromBytes IpHeader where
  decode = decodeBytesWith pIpv4Header

instance FromBytes IpPacket where
  decode = decodeBytesWith pIpv4Packet
