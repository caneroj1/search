module UninformedSearch.TestSlidingBlock
(
  testSlidingBlock
) where

import UninformedSearch.SlidingBlock
import Control.Search

testSlidingBlock = test initProblem

test :: SlidingBlockPuzzle -> IO ()
test p = do
  putStrLn "\n"
  putStrLn "Solving SlidingBlockPuzzle with A*"
  case astar p of
    Nothing -> putStrLn "Failure! No solution found!"
    Just p  -> print $ pathActions p
