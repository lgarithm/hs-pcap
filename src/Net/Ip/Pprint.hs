module Net.Ip.Pprint where

import           Misc.Utils     (show_chunks)
import           Net.Bits       (qword2words)
import           Net.Ip.Format  (IPv4Addr (IPv4Addr), IpHeader (identification, ipCheckSum, ipDstAddr, ipSrcAddr, protocol, ttl),
                                 IpPacket (IpPacket), ihl, ipFlags,
                                 ipPacketTotLen, offset, version)
import           Net.Tcp.Parse  (decodeTcpPacket)
import           Net.Tcp.Pprint ()
import           Text.Printf    (PrintfArg, formatArg, formatString, printf)

instance Show IPv4Addr where
  show (IPv4Addr ip) = printf "%d.%d.%d.%d" a b c d where [a,b,c,d] = qword2words ip

instance PrintfArg IPv4Addr where
  formatArg = formatString . show

ipProtoName 0x01 = "ICMP"
ipProtoName 0x02 = "IGMP"
ipProtoName 0x06 = "TCP"
ipProtoName 0x11 = "UDP"
ipProtoName _    = "?"

instance Show IpHeader where
  show hdr = unlines
    [ printf "IPv%d, hdrLen: %d" (version hdr) (ihl hdr)
    , printf "ipPacketTotLen: %d, identification: %d" (ipPacketTotLen hdr) (identification hdr)
    , printf "flags: %d, offset: %d, ttl: %d" (ipFlags hdr) (offset hdr) (ttl hdr)
    , let pn = protocol hdr
      in  printf "protocol %d (%s)" pn (ipProtoName pn)
    , printf "ipCheckSum 0x%04x" (ipCheckSum hdr)
    , printf "%s -> %s" (ipSrcAddr hdr) (ipDstAddr hdr)
    ]

showIpPacket p d = case p of
  0x06 -> show $ decodeTcpPacket d
  _    -> "non-tcp ip packet"

instance Show IpPacket where
  show (IpPacket h d) = unlines
    [ show h
    , show_chunks 8 d
    ]
