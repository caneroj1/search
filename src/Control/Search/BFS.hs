module Control.Search.BFS
(
  bfs
) where

import Control.Monad
import Control.Search.Internal.GraphSearch (isAtGoal)
import Control.Search.Internal.Path
import Control.Search.Internal.Frontier
import Control.Search.Internal.Queue
import Control.Search.Types
import Data.List
import Data.Maybe

bfs :: (Searchable a) => a -> Maybe (Path (State a) (Action a))
bfs p = search p (frontier p) emptySet
  where
    is       p = Node (initialState p) Nothing Nothing
    frontier p = mkFrontier `addFrontier` is p

search :: (Searchable a)
         => a
         -> Frontier Queue (State a) (Action a)
         -> ExploredSet a
         -> Maybe (Path (State a) (Action a))
search p frontier explored
  | isNothing mbf          = Nothing
  | isExplored st explored = search p frontier' explored
  | otherwise              = either Just doNextSearch children
  where
    mbf                    = headFrontier frontier
    Just (path, frontier') = mbf
    st                     = state path
    children               = makeChildren p path
    mkFrontier             = foldl' addFrontier frontier'
    doNextSearch cs        = search p (mkFrontier cs) (explore st explored)

makeChildren :: (Searchable a)
             => a
             -> Path (State a) (Action a)
             -> Either (Path (State a) (Action a))
                       (Level (State a) (Action a))
makeChildren p path = sequence $ do
  a <- actions st p
  s <- follows st a p
  let nc    = cost st a s p
      path' = Node s (Just a) nc -+- path
  if isAtGoal path' p then
    [Left path']      else
    [Right path']
  where
    st = state path
