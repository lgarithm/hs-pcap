module Net.Link.Format where

import           Data.Word (Word16, Word32, Word8)

data MacAddress = MacAddress Word8 Word8 Word8 Word8 Word8 Word8

data MacHeader = MacHeader { dstMacAddr :: MacAddress
                           , srcMacAddr :: MacAddress
                           , etherType  :: Word16
                           }

data Frame = Frame { macHeader :: MacHeader
                   , payload   :: [Word8]
                   , crc       :: Word32
                   }
