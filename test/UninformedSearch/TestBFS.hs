module UninformedSearch.TestBFS
(
  testBFS
) where

import UninformedSearch.Problem
import Control.Monad
import Control.Search.BFS
import Control.Search.Internal.Path

testBFS :: IO ()
testBFS = do
  putStrLn "\n"
  putStrLn "Testing BFS"
  putStrLn "-----------"
  case bfs initProblem of
    Nothing -> putStrLn "Failure! No solution found!"
    Just p  -> if p /= answer
                then do
                  putStrLn $ "Expected: " ++ show answer
                  putStrLn $ "Got:      " ++ show p
                else putStrLn "Pass!"

answer :: Path Location Drive
answer =
  Path Bucharest (Just Drive) (Just 211) $
    Path Fagaras (Just Drive) (Just 99)  $
      Node Sibiu Nothing Nothing
