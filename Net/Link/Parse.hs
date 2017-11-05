module Net.Link.Parse where

import           Misc.Binary                   (FromBytes (..))
import           Misc.Parse                    (anyByte, decodeBytesWith,
                                                nBytes, word16)
import           Net.Link.Format               (Frame (Frame),
                                                MacAddress (MacAddress),
                                                MacHeader (MacHeader))
import           Text.Parsec.ByteString        (Parser)
import           Text.ParserCombinators.Parsec (many)

pMacAddress = do { [o1, o2, o3, o4, o5, o6] <- nBytes 6
                 ; return $ MacAddress o1 o2 o3 o4 o5 o6
                 } :: Parser MacAddress

pMacHeader = do { dst <- pMacAddress
                ; src <- pMacAddress
                ; et <- word16
                ; return $ MacHeader dst src et
                } :: Parser MacHeader

pFrame = do { h <- pMacHeader
            ; t <- many anyByte
            -- ; let p = reverse t
--            ; d:c:b:a:p <- return $ reverse t
--            ; crc <- return $ fromIntegral . bytes2int 256 $ [a,b,c,d]
            ; let crc = 0 -- TODO
            ; return $ Frame h t crc
            } :: Parser Frame

instance FromBytes Frame where
  decode = decodeBytesWith pFrame
