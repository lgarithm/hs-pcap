{-# LANGUAGE StandaloneDeriving #-}

module Net.Tcp.Pprint where

import qualified Data.ByteString.Char8 as C8 (pack)
import           Data.Char             (chr)
import           Net.Tcp.Format
import           Text.Printf           (printf)

deriving instance Show TcpOffset
deriving instance Show TcpFlag
deriving instance Show TcpHeader
deriving instance Show TcpOptionalHeader

instance Show TcpPacket where
  show (TcpPacket th oh td) = unlines
    [ show th
    , show oh
    , printf "TcpBoby: %s" $ show (C8.pack $ map (chr . fromEnum) td)
    ]
