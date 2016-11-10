module Control.Search.BFS
(
  bfs
) where

import Control.Search.Internal.Path
import Control.Search.Internal.Frontier
import Control.Search.Types
import Data.List
import Data.Maybe

isAtGoal :: (Searchable a) => Path (State a) (Action a) -> a -> Bool
isAtGoal (Node a _ _)   = goal a
isAtGoal (Path a _ _ _) = goal a

bfs :: (Searchable a) => a -> Maybe (Path (State a) (Action a))
bfs p = search p (frontier p) emptySet
  where
    is       p = Node (initialState p) Nothing Nothing
    frontier p = mkFrontier `addFrontier` is p

search :: (Searchable a)
         => a
         -> Frontier (Path (State a) (Action a))
         -> ExploredSet a
         -> Maybe (Path (State a) (Action a))
search p frontier explored
  | isNothing mbf          = Nothing
  | isExplored st explored = search p frontier' explored
  | isAtGoal path p        = Just path
  | otherwise              = search p frontier'' (explore st explored)
  where
    mbf                    = headf frontier
    Just (path, frontier') = mbf
    st                     = state path
    frontier''             = foldl' addFrontier frontier' $! makeChildren p path

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
