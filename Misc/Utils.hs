module Misc.Utils where
import           Data.List.Split (chunksOf)
import           Text.Printf     (printf)

align m n = head . filter ((== 0) . flip mod m) $ [n .. ]

bytes2int n = foldl (carry n) 0 . map fromEnum where carry d x y = d * x + y

show_chunks n = unlines . map (unwords . map f) . chunksOf n where f n = printf " 0x%02x (%3d) " n n
