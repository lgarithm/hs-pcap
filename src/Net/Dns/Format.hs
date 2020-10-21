-- https://tools.ietf.org/html/rfc1035#section-4
module Net.Dns.Format where

import           Data.Word (Word16, Word32, Word8)

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

data Label = Label String | DnsPointer Word16

newtype QName = QName [Label]

-- https://tools.ietf.org/html/rfc1035#section-4.1.2
data DnsQuestionEntry = DnsQuestionEntry { qName  :: QName
                                         , qType  :: Word16
                                        --  https://tools.ietf.org/html/rfc1035#section-3.2.4
                                         , qClass :: Word16
                                         }

-- https://tools.ietf.org/html/rfc1035#section-4.1.3
data ResourceRecord = ResourceRecord { name     :: QName
                                     , _type    :: Word16
                                     , _class   :: Word16
                                     , dnsTtl   :: Word32
                                     , rdLength :: Word16
                                     , rdata    :: [Word8]
                                     }

data DnsMessage = DnsMessage { dnsHeader     :: DnsHeader
                             , dnsQuestion   :: [DnsQuestionEntry]
                             , dnsAnswer     :: [ResourceRecord]
                             , dnsAuthority  :: [ResourceRecord]
                             , dnsAdditional :: [ResourceRecord]
                             }
