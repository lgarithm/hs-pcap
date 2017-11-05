{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           App.Usage          (subUsage)
import           Control.Monad      ((>=>))
import           Data.Maybe         (mapMaybe)
import           Net.TCPIP          (ArpFrame, DhcpPacket, DnsMessage, Frame,
                                     IcmpPacket, IpPacket, TcpPacket, UdpPacket,
                                     icmpHeader, ipHeader, macHeader, tcpData)
import           Query              (Query, loadLinkPackets, query)
import           System.Environment (getArgs)


queries frames =
          [ (["eth"],            view (query :: Query Frame))
          , (["eth", "header"],  view ((query :: Query Frame) >=> Just . macHeader))
          , (["arp"],            view (query :: Query ArpFrame))
          , (["ip"],             view (query :: Query IpPacket))
          , (["ip", "header"],   view ((query:: Query IpPacket) >=> Just . ipHeader))
          , (["icmp"],           view (query :: Query IcmpPacket))
          , (["icmp", "header"], view ((query :: Query IcmpPacket) >=> Just . icmpHeader))
          , (["tcp"],            view (query :: Query TcpPacket))
          , (["tcp", "data"],    view ((query :: Query TcpPacket) >=> Just . tcpData))
          , (["udp"],            view (query :: Query UdpPacket))
          , (["dhcp"],           view (query :: Query DhcpPacket))
          , (["dns"],            view (query :: Query DnsMessage))
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
