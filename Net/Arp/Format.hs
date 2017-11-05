module Net.Arp.Format where

import           Data.Word       (Word16, Word32, Word8)
import           Net.Link.Format (MacAddress)

data ArpFrame = ArpFrame { hType :: Word16
                         , pType :: Word16
                         , hLen  :: Word8
                         , pLen  :: Word8
                         , oper  :: Word16
                         , sha   :: MacAddress
                         , spa   :: Word32
                         , tha   :: MacAddress
                         , tpa   :: Word32
                         }
