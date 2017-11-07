module Net.Dns.Parse where

import           Control.Monad           (replicateM)
import           Data.Bits               (shiftL, shiftR, (.&.), (.|.))
import           Data.Word               (Word16, Word8)
import           Language.Haskell.TH.Ppr (bytesToString)
import           Misc.Binary             (FromBytes (..))
import           Misc.Parse              (anyByte, decodeBytesWith, nBytes,
                                          word16, word32)
import           Net.Dns.Format          (DnsHeader (..),
                                          DnsMessage (DnsMessage),
                                          DnsQuestionEntry (DnsQuestionEntry),
                                          Label (..), QName (QName),
                                          ResourceRecord (ResourceRecord))
import           Text.Parsec.ByteString  (Parser)

pFlags flg =
  let qr = flg `shiftR` 15 == 1
      opcode = (flg `shiftR` 11) .&. 0x0f
      aa = flg `shiftR` 10 == 1
      tc = flg `shiftR` 9 == 1
      rd = flg `shiftR` 8 == 1
      ra = flg `shiftR` 7 == 1
      -- _z = (flg `shiftR` 4) .&. 0x07
      rcode = flg .&. 0x0f
  in ( toEnum $ fromEnum qr
     , toEnum $ fromEnum opcode
     , aa, tc, rd, ra
     , toEnum $ fromEnum rcode)

pDnsHeader = do { [_id, _flags, c1, c2, c3, c4] <- replicateM 6 word16
                ; let (qr, opcode, aa, tc, rd, ra, rcode) = pFlags _flags
                ; return $ DnsHeader _id qr opcode aa tc rd ra rcode c1 c2 c3 c4
                } :: Parser DnsHeader

pLabel = do { cnt <- anyByte;
            ; if cnt < 192
              then fmap (Label . bytesToString) (nBytes cnt)
              else do { lo <- anyByte;
                      ; let hi = (cnt .&. 63)
                      ; return $ DnsPointer $ makeWord16 hi lo
                      }
            } :: Parser Label

pLabels acc = do
  l <- pLabel
  case l of
    Label ""         -> return $ reverse acc
    l@(DnsPointer _) -> return $ reverse (l : acc)
    _                -> pLabels (l : acc)

pDomainName = do { qName <- pLabels []
                 ; return $ QName qName
                 } :: Parser QName

pDnsQuestionEntry = do { qName <- pDomainName
                       ; qType <- word16
                       ; qClass <- word16
                       ; return $ DnsQuestionEntry qName qType qClass
                       } :: Parser DnsQuestionEntry

pResourceRecord = do { name <- pDomainName
                     ; _type <- word16
                     ; _class <- word16
                     ; ttl <- word32
                     ; rdLength <- word16
                     ; rdata <- nBytes rdLength
                     ; return $ ResourceRecord name _type _class ttl rdLength rdata
                     } :: Parser ResourceRecord

pDnsMessage = do { header <- pDnsHeader
                 ; qd <- replicateM (fromEnum $ qdCount header) pDnsQuestionEntry
                 ; an <- replicateM (fromEnum $ anCount header) pResourceRecord
                 ; ns <- replicateM (fromEnum $ nsCount header) pResourceRecord
                 ; ar <- replicateM (fromEnum $ arCount header) pResourceRecord
                 ; return $ DnsMessage header qd an ns ar
                 } :: Parser DnsMessage

instance FromBytes DnsMessage where
  decode = decodeBytesWith pDnsMessage

makeWord16 :: Word8 -> Word8 -> Word16
makeWord16 hi lo =
  let hh = toEnum $ fromEnum hi :: Word16
      ll = toEnum $ fromEnum lo :: Word16
  in  (hh `shiftL` 8) .|. ll
