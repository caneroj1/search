module UninformedSearch.TestAStar
(
  testAStar
) where

import UninformedSearch.GetToBucharest
import qualified UninformedSearch.Problem1 as P1
import Control.Search

testAStar = test 1 P1.initProblem P1.aStarAnswer

test :: Int -> GetToBucharest -> Answer -> IO ()
test i p answer = do
  putStrLn    "\n"
  putStrLn $  "Testing A* [" ++ show i ++ "]"
  case astar p of
    Nothing -> putStrLn "Failure! No solution found!"
    Just p  -> if p /= answer
                then do
                  putStrLn $ "Expected: " ++ show answer
                  putStrLn $ "Got:      " ++ show p
                else putStrLn "Pass!"
