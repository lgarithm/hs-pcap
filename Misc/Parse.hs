module Misc.Parse
  ( ParseResult
  , anyByte
  , word16
  , word32
  , nBytes
  , nString
  , decodeBytesWith
  ) where

import           Control.Monad                      (replicateM)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Char8              as C8 (pack)
import           Data.ByteString.UTF8               (toString)
import           Data.Char                          (chr)
import           Data.Word                          (Word16, Word32, Word8)
import           Misc.Utils                         (bytes2int)
import           Text.Parsec.ByteString             (Parser)
import           Text.Parsec.Error                  (ParseError)
import           Text.Parsec.Prim                   (Parsec)
import           Text.ParserCombinators.Parsec      (parse)
import           Text.ParserCombinators.Parsec.Char (anyChar)

anyByte = fmap (fromIntegral . fromEnum) anyChar :: Parser Word8
pInt k = fmap (fromIntegral . bytes2int 256) (replicateM k anyByte)
word16 = pInt 2 :: Parser Word16
word32 = pInt 4 :: Parser Word32

bytes2str :: [Word8] -> ByteString
bytes2str = C8.pack . map (chr . fromEnum :: Word8 -> Char)

type ParseResult a = Either ParseError a

decodeBytesWith :: Parsec ByteString () a -> [Word8] -> ParseResult a
decodeBytesWith p = parse p "" . bytes2str

nString n = fmap (toString . C8.pack) (replicateM (fromEnum n) anyChar) :: Parser String

nBytes n = replicateM (fromEnum n) anyByte :: Parser [Word8]
