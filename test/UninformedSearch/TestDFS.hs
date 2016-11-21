module UninformedSearch.TestDFS
(
  testDFS
) where

import UninformedSearch.GetToBucharest
import qualified UninformedSearch.Problem1 as P1
import qualified UninformedSearch.Problem2 as P2
import Control.Search

testDFS = do
  test 1 P1.initProblem P1.dfsAnswer
  test 2 P2.initProblem P2.dfsAnswer

test :: Int -> GetToBucharest -> Answer -> IO ()
test i p answer = do
  putStrLn    "\n"
  putStrLn $  "Testing DFS [" ++ show i ++ "]"
  case dfs p of
    Nothing -> putStrLn "Failure! No solution found!"
    Just p  -> if p /= answer
                then do
                  putStrLn $ "Expected: " ++ show answer
                  putStrLn $ "Got:      " ++ show p
                else putStrLn "Pass!"
