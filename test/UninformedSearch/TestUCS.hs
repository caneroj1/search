module UninformedSearch.TestUCS
(
  testUCS
) where

import UninformedSearch.GetToBucharest
import qualified UninformedSearch.Problem1 as P1
import qualified UninformedSearch.Problem2 as P2
import Control.Search

testUCS = do
  test 1 P1.initProblem P1.ucsAnswer
  test 2 P2.initProblem P2.ucsAnswer

test :: Int -> GetToBucharest -> Answer -> IO ()
test i p answer = do
  putStrLn    "\n"
  putStrLn $  "Testing UCS [" ++ show i ++ "]"
  case uniform p of
    Nothing -> putStrLn "Failure! No solution found!"
    Just p  -> if p /= answer
                then do
                  putStrLn $ "Expected: " ++ show answer
                  putStrLn $ "Got:      " ++ show p
                else putStrLn "Pass!"
