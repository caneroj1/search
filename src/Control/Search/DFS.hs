module Control.Search.DFS
(
  dfs
, dfsLimited
, iterativeDeepening
, iterativeDeepeningLimited
, targetDepth
) where

import Control.Applicative
import Control.Monad
import Control.Search.Internal.Depth
import Control.Search.Internal.DepthSearch
import Control.Search.Internal.Path
import Control.Search.Internal.Frontier
import Control.Search.Internal.Stack
import Control.Search.Types
import Data.List
import Data.Maybe
import Data.Word

dfs :: (Searchable a) => a -> Maybe (Path (State a) (Action a))
dfs = dfsLimited NoLimit

dfsLimited :: (Searchable a) => Depth -> a -> Maybe (Path (State a) (Action a))
dfsLimited d p = depthSearch p d (frontier p) emptySet
  where
    is       p = WD (0, Node (initialState p) Nothing Nothing)
    frontier :: (Searchable a) => a -> Frontier Stack (State a) (Action a)
    frontier p = mkFrontier `addFrontier` is p

iterativeDeepening :: (Searchable a) => a -> Maybe (Path (State a) (Action a))
iterativeDeepening = iterativeDeepeningLimited NoLimit

iterativeDeepeningLimited :: (Searchable a)
                          => Depth
                          -> a
                          -> Maybe (Path (State a) (Action a))
iterativeDeepeningLimited md = go initialDepth
  where
    go d p
      | isAtDepthLimit d md = dfsLimited cd p
      | otherwise           = dfsLimited cd p <|> go (d+1) p
      where cd = targetDepth d
    initialDepth = 0
