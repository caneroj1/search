import UninformedSearch.TestBFS
import UninformedSearch.TestDFS
import UninformedSearch.TestDFSLimited
import UninformedSearch.TestUCS

main :: IO ()
main = testBFS        >>
       testDFS        >>
       testDFSLimited >>
       testUCS
