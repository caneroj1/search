module UninformedSearch.TestDFSLimited
(
  testDFSLimited
) where

import UninformedSearch.GetToBucharest
import qualified UninformedSearch.Problem1 as P1
import qualified UninformedSearch.Problem2 as P2
import Control.Search
import Data.Word

testDFSLimited = do
  test 1 0 P1.initProblem P1.depthAnswer
  test 1 1 P1.initProblem P1.depthAnswer
  test 1 2 P1.initProblem P1.depthAnswer
  test 1 3 P1.initProblem P1.depthAnswer
  test 2 0 P2.initProblem P2.depthAnswer
  test 2 1 P2.initProblem P2.depthAnswer
  test 2 2 P2.initProblem P2.depthAnswer
  test 2 3 P2.initProblem P2.depthAnswer

test :: Int -> Word32 -> GetToBucharest -> (Word32 -> Maybe Answer) -> IO ()
test i d p answerFn = do
  putStrLn    "\n"
  putStrLn $  "Testing DFSLimited [" ++ show i ++ "] [Depth " ++ show d ++ "]"
  let mbSolution = dfsLimited p (targetDepth d)
      mbAnswer   = answerFn d
  if mbAnswer /= mbSolution
    then do
      putStrLn $ "Expected: " ++ show mbAnswer
      putStrLn $ "Got:      " ++ show mbSolution
    else putStrLn "Pass!"
