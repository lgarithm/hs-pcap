module Net.Bits where
import           Data.Bits (shiftR)
import           Data.Word (Word16, Word32, Word8)

dword2words :: Word16 -> [Word8]
dword2words dw = map (fromIntegral . fromEnum) [dw `shiftR` 8, dw]

qword2dwords :: Word32 -> [Word16]
qword2dwords qw = map (fromIntegral . fromEnum) [qw `shiftR` 16, qw]

qword2words :: Word32 -> [Word8]
qword2words = concatMap dword2words . qword2dwords

class Binary a where
  encode :: a -> [Word8]
  decode :: [Word8] -> Either String a
