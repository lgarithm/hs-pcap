import           Misc.Test
import           Misc.Utils

t 1 = test_list "test_align" [ test_equal "" 4 (align 4 3)
                             , test_equal "" 12 (align 4 11)
                             , test_equal "" 100 (align 10 91)
                             ]

t 2 = test_list "test_bytes2int" [ test_equal "" 123 (bytes2int 10 [1,2,3])
                                 , test_equal "" 1 (bytes2int 256 [0,1])
                                 , test_equal "" 256 (bytes2int 256 [1,0])
                                 , test_equal "" 257 (bytes2int 256 [1,1])
                                 , test_equal "" 0xffffffff (bytes2int 256 $ replicate 4 0xff)
                                 ]

main = run_all_tests t 2
