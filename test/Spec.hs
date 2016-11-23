import UninformedSearch.TestBFS
import UninformedSearch.TestDFS
import UninformedSearch.TestDFSLimited

main :: IO ()
main = testBFS >>
       testDFS >>
       testDFSLimited
