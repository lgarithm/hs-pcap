module Net.PcapNg.Filters where

import           GHC.Word (Word16, Word8)
import           Net.Ip
import           Net.Link
import           Net.Tcp
import           Net.Udp

if' x y z = if x then y else z
just p q = if' p (Just q) Nothing
select predicate q = just (predicate q) q

-- https://en.wikipedia.org/wiki/EtherType
ether_type :: Word16 -> Frame -> Maybe [Word8]
ether_type et (Frame mh p _) = just (et == (etherType mh)) p

arp_frame, ip_frame :: Frame -> Maybe [Word8]
arp_frame = ether_type 0x0806
ip_frame = ether_type 0x0800

-- https://en.wikipedia.org/wiki/List_of_IP_protocol_numbers
ip_type :: Word8 -> IpPacket -> Maybe [Word8]
ip_type it (IpPacket h d) = just (it == (protocol h)) d

icmp_packet, tcp_packet, udp_packet :: IpPacket -> Maybe [Word8]
icmp_packet = ip_type 1
tcp_packet = ip_type 6
udp_packet = ip_type 17

-- https://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers
udp_type :: (UdpHeader -> Bool) -> UdpPacket -> Maybe [Word8]
udp_type p (UdpPacket hdr dat) = just (p hdr) dat

dns_packet, dhcp_packet :: UdpPacket -> Maybe [Word8]
dns_packet = udp_type p
  where p hdr = udpSrcPort hdr == 53 || udpDstPort hdr == 53

dhcp_packet = udp_type p
  where p hdr = udpDstPort hdr == 67 || udpDstPort hdr == 68

-- dhcp_discovery =

-- tcp
from_tcp_port :: Word16 -> TcpPacket -> Maybe Word16
from_tcp_port n = select (n==) . tcpSrcPort . tcpHeader
to_tcp_port n = select (n==) . tcpDstPort . tcpHeader
from_udp_port n = select (n==) . udpSrcPort . udpHeader
to_udp_port n = select (n==) . udpDstPort . udpHeader
