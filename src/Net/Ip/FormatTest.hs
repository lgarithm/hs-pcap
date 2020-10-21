module Net.Ip.FormatTest where

import           Misc.Test
import           Net.Ip.Format

test_versionAndIhl f h l  = test_list "test_versionAndIhl"
                              [ test_equal "" h (version ihv4)
                              , test_equal "" l (ihl ihv4)]
  where ihv4 = Ipv4Header f 1 2 3 4 5 6 7 (IPv4Addr 8) (IPv4Addr 9)

test_dscpAndECN f h l = test_list "test_dscpAndECN"
                          [ test_equal "" h (dscp ihv4)
                          , test_equal "" l (ecn ihv4)]
  where ihv4 = Ipv4Header 0 f 2 3 4 5 6 7 (IPv4Addr 8) (IPv4Addr 9)

test_ipFlagsAndOffset f h l  = test_list "test_ipFlagsAndOffset"
                               [ test_equal "" h (ipFlags ihv4)
                               , test_equal "" l (offset ihv4)]
  where ihv4 = Ipv4Header 0 1 2 3 f 5 6 7 (IPv4Addr 8) (IPv4Addr 9)

t 1 = test_versionAndIhl 1 0 1
t 2 = test_versionAndIhl 16 1 0
t 3 = test_dscpAndECN 1 0 1
t 4 = test_dscpAndECN 4 1 0
t 5 = test_ipFlagsAndOffset 1 0 1
t 6 = test_ipFlagsAndOffset 0x1000 0 0x1000
t 7 = test_ipFlagsAndOffset 0x2000 1 0

main = run_all_tests t 7
