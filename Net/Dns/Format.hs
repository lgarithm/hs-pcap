-- https://tools.ietf.org/html/rfc1035#section-4
module Net.Dns.Format where

import           Data.Word (Word16)

-- https://tools.ietf.org/html/rfc1035#section-4.1.1
data DnsHeader = DnsHeader { _id      :: Word16
                            , qr      :: Bool
                            , opCode  :: Word16 -- TODO: Word18
                            , _aa     :: Bool
                            , _tc     :: Bool
                            , _rd     :: Bool
                            , _ra     :: Bool
                            , _z      :: Word16 -- TODO: Word18
                            , rcode   :: Word16 -- TODO: Word18
                            , qnCount :: Word16
                            , anCount :: Word16
                            , nsCount :: Word16
                            , arCount :: Word16
                            }

data DnsQuestionEntry = DnsQuestionEntry { qName  :: [String]
                                         , qType  :: Word16
                                         , qClass :: Word16
                                         }

-- https://tools.ietf.org/html/rfc1035#section-4.1.2
newtype DnsQuestion = DnsQuestion { qEntries :: [DnsQuestionEntry] }

data DnsAnswer = DnsAnswer
data DnsAuthority = DnsAuthority
data DnsAdditional = DnsAdditional

data DnsMessage = DnsMessage { dnsHeader     :: DnsHeader
                             , dnsQuestion   :: DnsQuestion
                             , dnsAnswer     :: Maybe DnsAnswer
                             , dnsAuthority  :: Maybe DnsAuthority
                             , dnsAdditional :: Maybe DnsAdditional
                             }
