module Net.Ip.Bits where
import           Misc.Parse
import           Misc.Sure
import           Net.Bits
import           Net.Ip.Format
import           Net.Ip.Parse

_encode_ipv4_header (Ipv4Header o1 o2 o34 o56 o78 o9 oa obc (IPv4Addr q1) (IPv4Addr q2)) =
  [o1, o2] ++ dword2words o34 ++ dword2words o56 ++ dword2words o78 ++
  [o9, oa] ++ dword2words obc ++ qword2words q1 ++ qword2words q2

instance Binary IpHeader where
  encode = _encode_ipv4_header
  decode = Right . sure . decodeBytesWith pIpv4Header
