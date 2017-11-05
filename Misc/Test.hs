module Misc.Test where
import           Test.HUnit

test_list name = TestList . (map (TestLabel name))
test_equal name x y = TestCase (assertEqual name x y)

run_all_tests t n = mapM_ runTestTT $ map t [1 .. n]
