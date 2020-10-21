-- https://tools.ietf.org/html/rfc1541
module Net.Dhcp.Format where
import           Data.Word     (Word16, Word32, Word8)
import           Net.Ip.Format (IPv4Addr)

-- https://tools.ietf.org/html/rfc2132#section-9.6
data DHCPMessageType = INVALID_DHCP_TYPE -- 0
                     | DHCPDISCOVER
                     | DHCPOFFER
                     | DHCPREQUEST
                     | DHCPDECLINE
                     | DHCPACK
                     | DHCPNAK
                     | DHCPRELEASE
                     | DHCPINFORM
                     deriving (Enum)

-- https://tools.ietf.org/html/rfc2132
-- DHCP Options and BOOTP Vendor Extensions
data DhcpOption = DhcpRawOption { code :: Word8
                                , len  :: Word8
                                , str  :: [Word8]
                                }
                | Pad                                     -- code=0 https://tools.ietf.org/html/rfc2132#section-3.1
                | SubnetMask IPv4Addr                     -- code=1 https://tools.ietf.org/html/rfc2132#section-3.3
                | TimeOffset Word32                       -- code=2
                | RouterOption [IPv4Addr]                 -- code=3
                | TimeServerOption [IPv4Addr]             -- code=4
                | NameServerOption [IPv4Addr]             -- code=5
                | DomainNameServerOption [IPv4Addr]       -- code=6
                | LogServerOption [IPv4Addr]              -- code=7
                | CookieServerOption [IPv4Addr]           -- code=8
                | LPRServerOption [IPv4Addr]              -- code=9
                | ImpressServerOption [IPv4Addr]          -- code=10
                | ResourceLocationServerOption [IPv4Addr] -- code=11
                | HostNameOption String                   -- code=12
                | BootFileSizeOption Word16               -- code=13
                | MeritDumpFile String                    -- code=14
                | DomainName String                       -- code=15
                | SwapServer IPv4Addr                     -- code=16
                | RootPath String                         -- code=17
                | ExtensionsPath String                   -- code=18

                -- DHCP Extensions: https://tools.ietf.org/html/rfc2132#section-9
                | RequestedIPAddress IPv4Addr             -- code=50
                | IPAddressLeaseTime Word32               -- code=51
                | OptionOverload Word16                   -- code=52
                | DHCPMessageTypeOption DHCPMessageType   -- code=53
                | ServerIdentifier IPv4Addr               -- code=54
                | ParameterRequestList [Word8]            -- code=55
                | Message String                          -- code=56
                | MaximumDHCPMessageSize Word16           -- code=57
                -- ...
                | ClientIdentifier [Word8]                -- code=61
                -- ...
                | End                                     -- code=255 https://tools.ietf.org/html/rfc2132#section-3.2

data DhcpOptions = DhcpOptions { magic       :: Word32 -- 0x63825363
                               , dhcpOptions :: [DhcpOption]
                               }

data DhcpMessage = DhcpMessage { op      :: Word8
                               , htype   :: Word8
                               , hlen    :: Word8
                               , hops    :: Word8
                                 --
                               , xid     :: Word32
                                 --
                               , secs    :: Word16
                               , flags   :: Word16
                                 --
                               , ciAddr  :: IPv4Addr -- client IP address
                               , yiAddr  :: IPv4Addr -- your IP address
                               , siAddr  :: IPv4Addr -- server IP address
                               , giAddr  :: IPv4Addr -- gateway IP address switched by relay
                               , chAddr  :: (Word32, Word32, Word32, Word32)

                                 -- 192 octets of 0s. BOOTP legacy
                               , sname   :: [Word32] -- 64 Bytes
                               , file    :: [Word32] -- 128 Bytes

                               , options :: DhcpOptions
                               }

data DhcpPacket = DhcpRaw DhcpMessage
                | DhcpDiscover DhcpMessage
                | DhcpOffer DhcpMessage
                | DhcpRequest DhcpMessage
                | DhcpAck DhcpMessage
