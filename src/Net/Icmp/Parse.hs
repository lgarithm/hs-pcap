module Net.Icmp.Parse where

import           Misc.Binary            (FromBytes (..))
import           Misc.Parse             (anyByte, decodeBytesWith, word16,
                                         word32)
import           Net.Icmp.Format        (IcmpHeader (IcmpHeader),
                                         IcmpPacket (IcmpPacket))
import           Text.Parsec            (many)
import           Text.Parsec.ByteString (Parser)

pIcmpHeader = do
  t <- anyByte
  c <- anyByte
  sum <- word16
  rest <- word32
  return $ IcmpHeader t c sum rest :: Parser IcmpHeader

pIcmpPacket = do
  h <- pIcmpHeader
  d <- many anyByte
  return $ IcmpPacket h d :: Parser IcmpPacket

instance FromBytes IcmpHeader where
  decode = decodeBytesWith pIcmpHeader

instance FromBytes IcmpPacket where
  decode = decodeBytesWith pIcmpPacket
