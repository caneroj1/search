module UninformedSearch.TestBFS
(
  testBFS
) where

import UninformedSearch.GetToBucharest
import qualified UninformedSearch.Problem1 as P1
import qualified UninformedSearch.Problem2 as P2
import Control.Search.BFS

testBFS = do
  test 1 P1.initProblem P1.answer
  test 2 P2.initProblem P2.answer

test :: Int -> GetToBucharest -> Answer -> IO ()
test i p answer = do
  putStrLn    "\n"
  putStrLn $  "Testing BFS [" ++ show i ++ "]"
  case bfs p of
    Nothing -> putStrLn "Failure! No solution found!"
    Just p  -> if p /= answer
                then do
                  putStrLn $ "Expected: " ++ show answer
                  putStrLn $ "Got:      " ++ show p
                else putStrLn "Pass!"
