module Net.Dns.Parse where

import           Control.Monad                 (replicateM)
import           Data.Bits                     (shiftR, (.&.))
import           Language.Haskell.TH.Ppr       (bytesToString)
import           Misc.Binary                   (FromBytes (..))
import           Misc.Parse                    (anyByte, decodeBytesWith,
                                                word16)
import           Net.Dns.Format                (DnsHeader (DnsHeader, qnCount),
                                                DnsMessage (DnsMessage),
                                                DnsQuestion (DnsQuestion),
                                                DnsQuestionEntry (DnsQuestionEntry))
import           Text.ParserCombinators.Parsec (many)

pFlags flg =
  let qr = flg `shiftR` 15 == 1
      opcode = (flg `shiftR` 11) .&. 0x0f
      aa = flg `shiftR` 10 == 1
      tc = flg `shiftR` 9 == 1
      rd = flg `shiftR` 8 == 1
      ra = flg `shiftR` 7 == 1
      z = (flg `shiftR` 4) .&. 0x07
      rcode = flg .&. 0x0f
  in (qr, opcode, aa, tc, rd, ra, z, rcode)

pDnsHeader = do
  [_id, _flags, c1, c2, c3, c4] <- replicateM 6 word16
  let (qr, opcode, aa, tc, rd, ra, z, rcode) = pFlags _flags
  return $ DnsHeader _id qr opcode aa tc rd ra z rcode c1 c2 c3 c4

pLabels acc = do
  cnt <- anyByte
  octs <- replicateM (fromEnum cnt) anyByte
  if cnt == 0 then return $ reverse acc else pLabels $ (bytesToString octs) : acc

pDomainName = pLabels []

pDnsQuestionEntry = do
  qName <- pDomainName
  qType <- word16
  qClass <- word16
  return $ DnsQuestionEntry qName qType qClass

pDnsMessage = do
  header <- pDnsHeader
  qEntries <- replicateM (fromEnum $ qnCount header) pDnsQuestionEntry
  _bs <- many anyByte
  return $ DnsMessage header (DnsQuestion qEntries) Nothing Nothing Nothing

instance FromBytes DnsMessage where
  decode = decodeBytesWith pDnsMessage
