-- https://tools.ietf.org/html/rfc1035#section-4
module Net.Dns.Format where

import           Data.Word (Word16, Word8)

data QR = DnsQuery | DnsResponse deriving (Enum, Eq, Show)

-- https://tools.ietf.org/html/rfc1035#section-4.1.1
data DnsHeader = DnsHeader { _id      :: Word16
                            , qr      :: QR
                            , opCode  :: Word8
                            , _aa     :: Bool -- Authoritative Answer
                            , _tc     :: Bool -- TrunCation
                            , _rd     :: Bool -- Recursion Desired
                            , _ra     :: Bool -- Recursion Available
                            --- 3 padding zeros ---
                            , rcode   :: Word8
                            , qdCount :: Word16 -- number of questions
                            , anCount :: Word16 -- number of answers
                            , nsCount :: Word16 -- number of name servers
                            , arCount :: Word16 -- number of additional resources
                            }

newtype QName = QName [String]

data DnsQuestionEntry = DnsQuestionEntry { qName  :: QName
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
