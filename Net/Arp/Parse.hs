module Net.Arp.Parse where

import           Misc.Binary            (FromBytes, decode)
import           Misc.Parse             (anyByte, decodeBytesWith, word16,
                                         word32)
import           Net.Arp.Format         (ArpFrame (ArpFrame))
import           Net.Link.Parse         (pMacAddress)
import           Text.Parsec.ByteString (Parser)

pArpFrame = do { ht <- word16
               ; pt <- word16
               ; hl <- anyByte
               ; pl <- anyByte
               ; op <- word16
               ; sha <- pMacAddress
               ; spa <- word32
               ; tha <- pMacAddress
               ; tpa <- word32
               ; return $ ArpFrame ht pt hl pl op sha spa tha tpa
               } :: Parser ArpFrame

instance FromBytes ArpFrame where
  decode = decodeBytesWith pArpFrame
