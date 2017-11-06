{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           App.Usage          (subUsage)
import           Control.Monad      ((>=>))
import           Data.List          (intercalate)
import           Data.Maybe         (mapMaybe)
import           Misc.Plain         (plain)
import           Net.TCPIP          (ArpFrame, DhcpPacket, DnsMessage,
                                     DnsQuestion, Frame, IcmpPacket, IpPacket,
                                     QName, TcpPacket, UdpPacket, icmpHeader,
                                     ipHeader, macHeader, qEntries, qName,
                                     tcpData)
import           Query              (Query, loadLinkPackets, query)
import           System.Environment (getArgs)


queries frames =
          [ (["eth"],              view (query :: Query Frame))
          , (["eth", "header"],    view ((query :: Query Frame) >=> Just . macHeader))
          , (["arp"],              view (query :: Query ArpFrame))
          , (["ip"],               view (query :: Query IpPacket))
          , (["ip", "header"],     view ((query:: Query IpPacket) >=> Just . ipHeader))
          , (["icmp"],             view (query :: Query IcmpPacket))
          , (["icmp", "header"],   view ((query :: Query IcmpPacket) >=> Just . icmpHeader))
          , (["tcp"],              view (query :: Query TcpPacket))
          , (["tcp", "data"],      view ((query :: Query TcpPacket) >=> Just . tcpData))
          , (["udp"],              view (query :: Query UdpPacket))
          , (["dhcp"],             view (query :: Query DhcpPacket))
          , (["dns"],              view (query :: Query DnsMessage))
          , (["dns", "questions"], view ((query :: Query DnsQuestion) >=>
                                         Just . (map qName . qEntries :: DnsQuestion -> [QName]) >=>
                                         Just . plain . intercalate "," . map show))
          ] :: [([String], IO())]
    where
        view q = mapM_ print . mapMaybe q $ frames

usage = subUsage [ "<file>" : cmds | cmds <- map fst (queries []) ]

main = do
    args <- getArgs
    case args of
      file : qs -> do
        frames <- loadLinkPackets file
        let mrun = lookup qs (queries frames)
        case mrun of
          Just run -> run
          _        -> usage
      _         -> usage
