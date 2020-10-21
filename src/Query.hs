{-# LANGUAGE ScopedTypeVariables #-}

module Query where

import           Control.Monad          ((>=>))
import           Data.Maybe             (mapMaybe)
import           Data.Word              (Word8)
import           Misc.Binary            (justDecode)
import           Misc.Sure              (sure)
import           Net.Pcap.Format        (Hex32 (Hex32), PcapFile (PcapFile),
                                         magic, packetData)
import           Net.Pcap.Parse         as Pcap (pGlobalHeader, pPcapFromFile)
import           Net.PcapNg.Filters     (arp_frame, dhcp_packet, dns_packet,
                                         icmp_packet, ip_frame, tcp_packet,
                                         udp_packet)
import           Net.PcapNg.Format      (Body (EnhancedPacketBody), PcapNGFile,
                                         blockBody, blocks)
import           Net.PcapNg.Parse       (pPcapNGFormatFromFile)
import           Net.TCPIP              (ArpFrame, DhcpPacket, DnsMessage,
                                         DnsQuestionEntry, Frame, IcmpPacket,
                                         IpPacket, TcpPacket, UdpPacket,
                                         dnsQuestion)
import           Text.Parsec.ByteString (parseFromFile)

type LinkPacket = [Word8]

class LinkEncapsulationFormat a where
  linkPackets :: a -> [LinkPacket]

instance LinkEncapsulationFormat PcapNGFile where
  linkPackets = mapMaybe (linkPacket . blockBody) . blocks
    where
        linkPacket (EnhancedPacketBody _ _ _ cl _ pd _) = Just $ take (fromEnum cl) pd
        linkPacket _                                    = Nothing

instance LinkEncapsulationFormat PcapFile where
  linkPackets (PcapFile _ blocks) = map packetData blocks

data PcapFormat = PCAP | PCAPNG deriving (Eq, Show)

pcapFileType :: FilePath -> IO PcapFormat
pcapFileType file = do
    mgHdr <- parseFromFile pGlobalHeader file
    case mgHdr of
      Right gHdr -> case magic gHdr of
        Hex32 0xa1b2c3d4 -> return PCAP
        _                -> return PCAPNG
      _         -> return PCAPNG

loadLinkPackets :: FilePath -> IO [LinkPacket]
loadLinkPackets file = do
    t <- pcapFileType file
    case t of
      PCAP   -> fmap (linkPackets . sure) (pPcapFromFile file)
      PCAPNG -> fmap (linkPackets . sure) (pPcapNGFormatFromFile file)

type Query a = LinkPacket -> Maybe a

class Queryable a where
  query :: Query a

instance Queryable Frame where
  query = justDecode

instance Queryable ArpFrame where
  query = (query :: Query Frame) >=> arp_frame >=> justDecode

instance Queryable IpPacket where
  query = (query :: Query Frame) >=> ip_frame >=> justDecode

instance Queryable IcmpPacket where
  query = (query :: Query IpPacket) >=> icmp_packet >=> justDecode

instance Queryable TcpPacket where
  query = (query :: Query IpPacket) >=> tcp_packet >=> justDecode

instance Queryable UdpPacket where
  query = (query :: Query IpPacket) >=> udp_packet >=> justDecode

instance Queryable DhcpPacket where
  query = (query :: Query UdpPacket) >=> dhcp_packet >=> justDecode

instance Queryable DnsMessage where
  query = (query :: Query UdpPacket) >=> dns_packet >=> justDecode

newtype DnsQuestion = DnsQuestion { qEntries :: [DnsQuestionEntry] }

instance Queryable DnsQuestion where
  query = (query :: Query DnsMessage) >=> Just . DnsQuestion . dnsQuestion
