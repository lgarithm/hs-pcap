module Net.AllTest where
import qualified Net.Ip.BitsTest   as T2
import qualified Net.Ip.FormatTest as T1

main = T1.main >> T2.main
