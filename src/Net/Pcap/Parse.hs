module Net.Pcap.Parse where

import           Control.Monad          (replicateM)
import qualified Data.ByteString        as BS (readFile)
import           Data.Word.Compat       (byteSwap16, byteSwap32)
import           Misc.Parse             (nBytes, word16, word32)
import           Net.Pcap.Format        (Block (Block),
                                         GlobalHeader (GlobalHeader),
                                         Hex32 (Hex32),
                                         PacketHeader (PacketHeader),
                                         PcapFile (PcapFile), inclLen)
import           Text.Parsec            (many, parse)
import           Text.Parsec.ByteString (Parser)

pGlobalHeader :: Parser GlobalHeader
pGlobalHeader = do
  magic <- fmap (Hex32 . byteSwap32) word32
  versionMajor <- fmap byteSwap16 word16
  versionMinor <- fmap byteSwap16 word16
  thisZone <- word32
  sigfigs <- word32
  snaplen <- fmap byteSwap32 word32
  network <- fmap byteSwap32 word32
  return $
    GlobalHeader
      magic
      versionMajor
      versionMinor
      thisZone
      sigfigs
      snaplen
      network

pPacketHeader = do
  [tsSec, tsUsec, inclLen, origLen] <- replicateM 4 (fmap byteSwap32 word32)
  return $ PacketHeader tsSec tsUsec inclLen origLen :: Parser PacketHeader

pBlock = do
  packetHeader <- pPacketHeader
  packetData <- nBytes (inclLen packetHeader)
  return $ Block packetHeader packetData :: Parser Block

pPcapFile = do
  globalHeader <- pGlobalHeader
  blocks <- many pBlock
  return $ PcapFile globalHeader blocks :: Parser PcapFile

pPcapFromFile file = fmap (parse pPcapFile file) (BS.readFile file)
