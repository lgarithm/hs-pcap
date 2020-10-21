module Net.PcapNg.Parse where

import qualified Data.ByteString        as BS (readFile)
import qualified Data.ByteString.Char8  as C8 (pack)
import           Data.Word.Compat       (byteSwap16, byteSwap32)
import           Misc.Parse             (anyByte, decodeBytesWith, nBytes,
                                         word16, word32)
import           Misc.Sure              (sure)
import           Misc.Utils             (align)
import           Net.PcapNg.Format
import           Text.Parsec            (many, parse)
import           Text.Parsec.ByteString (Parser)
import           Text.Parsec.Char       (anyChar)

pOption = do
  code <- word16
  length <- fmap byteSwap16 word16
  value <- nBytes (align 4 $ fromEnum length)
  return $ Option code length value :: Parser Option

pSectionHeaderBody :: Parser Body
pSectionHeaderBody = do
  byteOrderMagic <- word32
  majorVersion <- word16
  minorVersion <- word16
  sectionLength1 <- word32
  sectionLength2 <- word32
  options <- many word32
  return $
    SectionHeaderBody
      byteOrderMagic
      majorVersion
      minorVersion
      sectionLength1
      sectionLength2
      options

pInterfaceDescriptionBody = do
  linkType <- word16
  reserved <- word16
  snapLen <- word32
  options <- many word32
  return $ InterfaceDescriptionBody linkType reserved snapLen options :: Parser Body

pEnhancedPacketBody :: Parser Body
pEnhancedPacketBody = do
  interfaceID <- word32
  timestampHigh <- word32
  timestampLow <- word32
  capturedLen <- fmap byteSwap32 word32
  packetLen <- fmap byteSwap32 word32
  let len = align 4 . fromEnum $ capturedLen
  packetData <- nBytes len
  rawOption <- many anyChar
  let options = sure $ parse (many pOption) "" $ C8.pack rawOption
  return $
    EnhancedPacketBody
      interfaceID
      timestampHigh
      timestampLow
      capturedLen
      packetLen
      packetData
      options

pBody 0x00000001 = pInterfaceDescriptionBody
pBody 0x00000006 = pEnhancedPacketBody
pBody 0x0A0D0D0A = pSectionHeaderBody
pBody _          = fmap Raw (many anyByte) :: Parser Body

pBlock = do
  bType <- fmap byteSwap32 word32
  bLength <- fmap byteSwap32 word32
  let len = fromEnum bLength - 12
  bData <- nBytes len
  _tLength <- word32
  let body = sure $ decodeBytesWith (pBody bType) bData
  return $ Block bType bLength body :: Parser Block

pPcapNGFormat = fmap PcapNGFile (many pBlock) :: Parser PcapNGFile

pPcapNGFormatFromFile file = fmap (parse pPcapNGFormat file) (BS.readFile file)
