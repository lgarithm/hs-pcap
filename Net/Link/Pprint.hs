module Net.Link.Pprint where

import           Misc.Utils      (show_chunks)
import           Net.Link.Format (Frame (Frame), MacAddress (MacAddress),
                                  MacHeader (MacHeader))
import           Text.Printf     (PrintfArg, formatArg, formatString, printf)

instance Show MacAddress where
  show (MacAddress o1 o2 o3 o4 o5 o6) = printf "%02x:%02x:%02x:%02x:%02x:%02x"
                                        o1 o2 o3 o4 o5 o6

instance PrintfArg MacAddress where
  formatArg = formatString . show

ethTypeName 0x0800 = "IP"
ethTypeName 0x0806 = "ARP"
ethTypeName _      = "?"

instance Show MacHeader where
  show (MacHeader dst src et) = printf "%s <- %s 0x%04x (%s)" dst src et (ethTypeName et)

instance Show Frame where
  show (Frame h p c) = unlines
    [ printf "mac header: %s" (show h)
    , printf "crc 0x%08x" c
    , "Payload:"
    , printf "%s" (show_chunks 4 p)
    ]
