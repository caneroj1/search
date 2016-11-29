import Control.Monad
import Control.Search
import Data.Word
import qualified Test.HUnit as H
import SearchTests.Utils
import SearchTests.Problems.GetToBucharest
import qualified SearchTests.Problems.GetToBucharest.Problem1 as P1
import qualified SearchTests.Problems.GetToBucharest.Problem2 as P2
import SearchTests.Problems.SlidingBlock
import qualified UnitTests.MapSetUnitTests as MS
import qualified UnitTests.PathFunctionsUnitTests as PS
import qualified UnitTests.WeightsUnitTests as WS

unitTests :: H.Test
unitTests = H.TestList $ MS.unitTests ++
                         WS.unitTests ++
                         PS.unitTests

getToBucharestTests :: [Suite Location Drive]
getToBucharestTests = [
    foldr addToSuite suite1 [
        Test "BFS #1" P1.initProblem P1.bfsAnswer bfs
      , Test "BFS #2" P1.initProblem P1.bfsAnswer bfs
      , Test "DFS #1" P1.initProblem P1.dfsAnswer dfs
      , Test "DFS #2" P1.initProblem P1.dfsAnswer dfs
      , Test "DFS Limited #1 D0" P1.initProblem (P1.depthAnswer 0) (dfsl 0)
      , Test "DFS Limited #1 D1" P1.initProblem (P1.depthAnswer 1) (dfsl 1)
      , Test "DFS Limited #1 D2" P1.initProblem (P1.depthAnswer 2) (dfsl 2)
      , Test "DFS Limited #1 D3" P1.initProblem (P1.depthAnswer 3) (dfsl 3)
      , Test "DFS Limited #2 D0" P2.initProblem (P2.depthAnswer 0) (dfsl 0)
      , Test "DFS Limited #2 D1" P2.initProblem (P2.depthAnswer 1) (dfsl 1)
      , Test "DFS Limited #2 D2" P2.initProblem (P2.depthAnswer 2) (dfsl 2)
      , Test "DFS Limited #2 D3" P2.initProblem (P2.depthAnswer 3) (dfsl 3)
    ],
    foldr addToSuite suite2 [
        Test "Greedy #1" P1.initProblem P1.greedyAnswer greedy
      , Test "UCS #1" P1.initProblem P1.ucsAnswer uniform
      , Test "UCS #2" P2.initProblem P2.ucsAnswer uniform
      , Test "A* #1" P1.initProblem P1.aStarAnswer astar
    ]
  ]
  where
    suite1 = mkSuite "GetToBucharest - Uninformed"
    suite2 = mkSuite "GetToBucharest - Informed"
    dfsl d = dfsLimited (targetDepth d)

runTests :: IO ()
runTests = void $ H.runTestTT unitTests

main :: IO ()
main = forM_ getToBucharestTests runSuite >>
       putStrLn "Running Unit Tests"      >>
       runTests
