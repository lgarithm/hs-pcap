module Net.PcapNg.Pprint where

import           Data.Word.Compat  (byteSwap16, byteSwap32)
import           Misc.Utils        (show_chunks)
import           Net.PcapNg.Format
import           Text.Printf       (printf)

instance Show Option where
  show (Option code length value) = printf "code: %04x length: %d value: %s" code (byteSwap16 length) (show value)

instance Show Body where
  show (SectionHeaderBody b v vv l1 l2 o) = unlines
    [ "SectionHeaderBody"
    , printf "byteOrderMagic: 0x%08x" b
    , printf "majorVersion: %d" v
    , printf "minorVersion: %d" vv
    , printf "sectionLength: 0x%08x 0x%08x" l1 l2
    , printf "options: %s" (show o)
    ]
  show (InterfaceDescriptionBody l r sl o) = unlines
    [ "InterfaceDescriptionBody"
    , printf "linkType: 0x%04x" (byteSwap16 l)
    , printf "reserved: 0x%04x" r
    , printf "snapLen: %d" sl
    , printf "options: %s" (show o)
    ]
  show (EnhancedPacketBody i t1 t2 cl pl pd o) = unlines
    [ "EnhancedPacketBody"
    , printf "interfaceID: %08x" i
    , printf "time: %d %d" t1 t2
    , printf "capturedLen: %d" cl
    , printf "packetLen: %d" pl
    , printf "package data:\n%s" (show_chunks 4 pd)
    , printf "options:\n%s" (unlines . map show $ o)
    ]
  show (Raw bs) = unlines ["raw block:", show_chunks 4 bs]
  show _ = "unparsed block"

instance Show Block where
  show (Block t l b) = unlines [ printf "type: 0x%08x length: %d"
                                 (byteSwap32 t) (byteSwap32 l)
                                , (show b)]

instance Show PcapNGFile where
  show (PcapNGFile bs) = unlines $ map show bs
