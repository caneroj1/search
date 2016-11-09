module Control.Search.BFS
(
  bfs
) where

import Control.Search.Internal.Path
import Control.Search.Internal.Queue
import Control.Search.Types

isAtGoal :: (Searchable a) => Path (State a) (Action a) -> a -> Bool
isAtGoal (Node a _ _)   = goal a
isAtGoal (Path a _ _ _) = goal a

bfs :: (Searchable a) => a -> Maybe (Path (State a) (Action a))
bfs p = runLevel p [is]
  where
    is              = Node (initialState p) Nothing Nothing
    -- initialFrontier = is `insert` Seq.empty

runLevel :: (Searchable a)
         => a
         -> Level (State a) (Action a)
         -> Maybe (Path (State a) (Action a))
runLevel _ []    = Nothing
runLevel p level =
  either Just (runLevel p) $ concat <$> traverse visit level
  where
    visit path
      | isAtGoal path p = Left path
      | otherwise       = Right $ makeChildren p path

makeChildren :: (Searchable a)
             => a
             -> Path (State a) (Action a)
             -> Level (State a) (Action a)
makeChildren p path = do
  a <- actions st p
  s <- follows st a p
  let nc = cost st a s p
    in [Node s (Just a) nc -+- path]
  where
    st = state path
