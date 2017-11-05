module Net.Ip.BitsTest where
import           Misc.Test
import           Net.Bits
import           Net.Ip.Bits
import           Net.Ip.Format

t 1  = test_list "test_ipv4_header_length" [ test_equal "" 20 (length . encode $ ihv4) ]
  where ihv4 = Ipv4Header 0 1 2 3 4 5 6 7 (IPv4Addr 8) (IPv4Addr 9)

main = run_all_tests t 1
