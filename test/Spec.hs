import Control.Monad
import Test.HUnit
import UninformedSearch.TestBFS
import UninformedSearch.TestDFS
import UninformedSearch.TestDFSLimited
import UninformedSearch.TestUCS
import qualified UnitTests.MapSetUnitTests as MS
import qualified UnitTests.WeightsUnitTests as WS

unitTests :: Test
unitTests = TestList $ MS.unitTests ++ WS.unitTests

runTests :: IO ()
runTests = void $ runTestTT unitTests

main :: IO ()
main = testBFS        >>
       testDFS        >>
       testDFSLimited >>
       testUCS        >>
       runTests
