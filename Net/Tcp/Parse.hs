module Net.Tcp.Parse where

import           Data.Bits              (shiftR, (.&.))
import           Data.Word              (Word16)
import           Misc.Binary            (FromBytes (..))
import           Misc.Parse             (anyByte, decodeBytesWith, nBytes,
                                         word16, word32)
import           Misc.Sure              (sure)
import           Net.Tcp.Format
import           Text.Parsec            (many)
import           Text.Parsec.ByteString (Parser)

_tcpOffset =
  TcpOffset . fromIntegral . fromEnum . (`shiftR` 12) :: Word16 -> TcpOffset

_tcpFlags _ofs = [f | f <- [NS ..], (tcpFlagMask f .&. _ofs) /= 0]

pTcpHeader = do
  sp <- word16
  dp <- word16
  seq <- word32
  ack <- word32
  _ofs <- word16
  let (offset, flags) = (_tcpOffset _ofs, _tcpFlags _ofs)
  ws <- word16
  cs <- word16
  up <- word16
  return $ TcpHeader dp sp seq ack offset flags ws cs up :: Parser TcpHeader

pTcpOptionalHeader offset =
  fmap TcpOptionalHeader (nBytes offset) :: Parser TcpOptionalHeader

pTcpPacket = do
  th <- pTcpHeader
  oh <-
    let (TcpOffset len) = offset th
     in if len > 5
          then fmap Just (pTcpOptionalHeader $ (fromEnum len - 5) * 4)
          else return Nothing
  td <- many anyByte
  return $ TcpPacket th oh td :: Parser TcpPacket

decodeTcpPacket = sure . decodeBytesWith pTcpPacket

instance FromBytes TcpHeader where
  decode = decodeBytesWith pTcpHeader

instance FromBytes TcpPacket where
  decode = decodeBytesWith pTcpPacket
