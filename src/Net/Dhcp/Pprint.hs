{-# LANGUAGE StandaloneDeriving #-}
module Net.Dhcp.Pprint where
import           Net.Dhcp.Format
import           Net.Ip.Pprint   ()
import           Text.Printf     (printf)

deriving instance Show DHCPMessageType
deriving instance Show DhcpOption
deriving instance Show DhcpOptions
deriving instance Show DhcpPacket

instance Show DhcpMessage where
  show (DhcpMessage
         op htype hlen hopts
         xid
         secs flags
         ciAddr
         yiAddr
         siAddr
         giAddr
         _chAddr
         _sname
         _file
         _opts
       ) =
    let opts = filter (\opts -> case opts of Pad -> False; End -> False; _ -> True) $ dhcpOptions _opts
    in  unlines $
    [ printf "op: 0x%02x, htype: 0x%02x, hlen: 0x%02x, hops: 0x%02x" op htype hlen hopts
    , printf "xid: 0x%08x" xid
    , printf "secs: 0x%04x, flags: 0x%04x" secs flags
    , "ciAddr: " ++ show ciAddr
    , "yiAddr: " ++ show yiAddr
    , "siAddr: " ++ show siAddr
    , "giAddr: " ++ show giAddr
    -- chAddr
    , show (length opts) ++ " options"
    , printf "magic: 0x%08x" (magic _opts)
    ] ++ map show opts
