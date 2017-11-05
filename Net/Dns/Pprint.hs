{-# LANGUAGE StandaloneDeriving #-}

module Net.Dns.Pprint where

import           Net.Dns.Format (DnsAdditional (DnsAdditional),
                                 DnsAnswer (DnsAnswer),
                                 DnsAuthority (DnsAuthority),
                                 DnsHeader (DnsHeader), DnsMessage (DnsMessage),
                                 DnsQuestion (DnsQuestion),
                                 DnsQuestionEntry (DnsQuestionEntry))

deriving instance Show DnsMessage
deriving instance Show DnsHeader
deriving instance Show DnsAnswer
deriving instance Show DnsQuestion
deriving instance Show DnsAuthority
deriving instance Show DnsAdditional
deriving instance Show DnsQuestionEntry
