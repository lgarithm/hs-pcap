module Net.Dhcp.Parse where

import           Control.Monad          (replicateM)
import           Data.Word              (Word8)
import           Misc.Binary            (FromBytes (..))
import           Misc.Parse             (anyByte, decodeBytesWith, nBytes,
                                         nString, word16, word32)
import           Net.Dhcp.Format        (DHCPMessageType (..),
                                         DhcpMessage (DhcpMessage),
                                         DhcpOption (..),
                                         DhcpOptions (DhcpOptions),
                                         DhcpPacket (DhcpRaw))
import           Net.Ip.Parse           (pIPv4Addr)
import           Text.Parsec            (many)
import           Text.Parsec.ByteString (Parser)

pDHCPMessageType :: Parser DHCPMessageType
pDHCPMessageType = do
  byte <- anyByte
  return (toEnum . fromEnum $ byte :: DHCPMessageType)

pDhcpOptionWithCode :: Word8 -> Parser DhcpOption
pDhcpOptionWithCode code = do
  len <- anyByte
  case code of
    1  -> fmap SubnetMask pIPv4Addr
    2  -> fmap TimeOffset word32
    3  -> fmap RouterOption (many pIPv4Addr)
    4  -> fmap TimeServerOption (many pIPv4Addr)
    5  -> fmap NameServerOption (many pIPv4Addr)
    6  -> fmap DomainNameServerOption (many pIPv4Addr)
    7  -> fmap LogServerOption (many pIPv4Addr)
    8  -> fmap CookieServerOption (many pIPv4Addr)
    9  -> fmap LPRServerOption (many pIPv4Addr)
    10 -> fmap ImpressServerOption (many pIPv4Addr)
    11 -> fmap ResourceLocationServerOption (many pIPv4Addr)
    12 -> fmap HostNameOption (nString len)
    50 -> fmap RequestedIPAddress pIPv4Addr
    51 -> fmap IPAddressLeaseTime word32
    52 -> fmap OptionOverload word16
    53 -> fmap DHCPMessageTypeOption pDHCPMessageType
    54 -> fmap ServerIdentifier pIPv4Addr
    55 -> fmap ParameterRequestList (nBytes len)
    56 -> fmap Message (nString len)
    57 -> fmap MaximumDHCPMessageSize word16
    61 -> fmap ClientIdentifier (nBytes len)
    _  -> fmap (DhcpRawOption code len) (nBytes len)

pDhcpOption = do
  code <- anyByte
  case code of
    0   -> return Pad
    255 -> return End
    _   -> pDhcpOptionWithCode code :: Parser DhcpOption

pDhcpOptions = do
  magic <- word32
  options <- many pDhcpOption
  return $ DhcpOptions magic options :: Parser DhcpOptions

pDhcpMessage :: Parser DhcpMessage
pDhcpMessage = do
  [op, htype, hlen, hops] <- replicateM 4 anyByte
  xid <- word32
  [secs, flags] <- replicateM 2 word16
  [ciAddr, yiAddr, siAddr, giAddr] <- replicateM 4 pIPv4Addr
  [ch1, ch2, ch3, ch4] <- replicateM 4 word32
  sname <- replicateM 16 word32
  file <- replicateM 32 word32
  options <- pDhcpOptions
  return $
    DhcpMessage
      op
      htype
      hlen
      hops
      xid
      secs
      flags
      ciAddr
      yiAddr
      siAddr
      giAddr
      (ch1, ch2, ch3, ch4)
      sname
      file
      options

pDhcpPacket = do
  m <- pDhcpMessage
  return $ DhcpRaw m :: Parser DhcpPacket

instance FromBytes DhcpPacket where
  decode = decodeBytesWith pDhcpPacket
