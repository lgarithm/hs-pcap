{-# LANGUAGE StandaloneDeriving #-}

module Net.Dns.Pprint where

import           Data.List      (intercalate)
import           Net.Dns.Format (DnsHeader (DnsHeader), DnsMessage (DnsMessage),
                                 DnsQuestionEntry (DnsQuestionEntry),
                                 Label (..), QName (QName),
                                 ResourceRecord (ResourceRecord))
import           Text.Printf    (printf)

deriving instance Show DnsHeader
deriving instance Show DnsQuestionEntry
deriving instance Show ResourceRecord

instance Show Label where
    show (Label s)        = s
    show (DnsPointer off) = printf "@%d" off

instance Show QName where
    show (QName labels) = intercalate "." $ map show labels

instance Show DnsMessage where
    show (DnsMessage hdr qd an ns ar) = unlines $
        [(show hdr)] ++
        (map show qd) ++
        (map show an) ++
        (map show ns) ++
        (map show ar)
