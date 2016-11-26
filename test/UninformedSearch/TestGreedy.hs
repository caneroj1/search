module UninformedSearch.TestGreedy
(
  testGreedy
) where

import UninformedSearch.GetToBucharest
import qualified UninformedSearch.Problem1 as P1
import Control.Search

testGreedy = test 1 P1.initProblem P1.greedyAnswer

test :: Int -> GetToBucharest -> Answer -> IO ()
test i p answer = do
  putStrLn    "\n"
  putStrLn $  "Testing Greedy [" ++ show i ++ "]"
  case greedy p of
    Nothing -> putStrLn "Failure! No solution found!"
    Just p  -> if p /= answer
                then do
                  putStrLn $ "Expected: " ++ show answer
                  putStrLn $ "Got:      " ++ show p
                else putStrLn "Pass!"
