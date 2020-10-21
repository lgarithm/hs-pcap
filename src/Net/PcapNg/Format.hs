module Net.PcapNg.Format where

import           Data.Word (Word16, Word32, Word8)

data Option = Option { optionCode   :: Word16
                     , optionLength :: Word16
                     , optionValue  :: [Word8]
                     }

data Body = Raw [Word8]
          | SectionHeaderBody { byteOrderMagic :: Word32
                              , majorVersion   :: Word16
                              , minorVersion   :: Word16
                              , sectionLength1 :: Word32
                              , sectionLength2 :: Word32
                              , rawOptions     :: [Word32]
                              }
          | InterfaceDescriptionBody { linkType   :: Word16
                                     , reserved   :: Word16
                                     , snapLen    :: Word32
                                     , rawOptions :: [Word32]
                                     }
          | EnhancedPacketBody { interfaceID   :: Word32
                               , timestampHigh :: Word32
                               , timestampLow  :: Word32
                               , capturedLen   :: Word32
                               , packetLen     :: Word32
                               , packetData    :: [Word8]
                               , options       :: [Option]
                               }
          | SimplePacketBlock { simplePacketLen  :: Word32
                              , simplePacketData :: [Word32]
                              }
          | PacketBlock [Word32] -- obsolete
          | NameResolutionBlock

data Block = Block { blockType   :: Word32
                   , blockLength :: Word32
                   , blockBody   :: Body
                   }

newtype PcapNGFile = PcapNGFile { blocks :: [Block] }

newtype PcapNGFileInfo = PcapNGFileInfo { numOfBlocks :: Int } deriving (Show)

pcapInfo (PcapNGFile blocks) = PcapNGFileInfo (length blocks)
