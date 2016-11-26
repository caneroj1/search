module Control.Search.DFS
(
  dfs
, dfsLimited
, iterativeDeepening
, iterativeDeepeningLimited
, targetDepth
) where

import Control.Applicative
import Control.Search.Internal.Depth
import Control.Search.Internal.DepthSearch
import qualified Control.Search.Internal.ExploredSet as E
import Control.Search.Internal.Path
import Control.Search.Internal.Frontier
import Control.Search.Internal.Stack
import Control.Search.Types

dfs :: (Ord state) => Searchable state action -> Maybe (Path state action)
dfs p = dfsLimited p NoLimit

dfsLimited :: (Ord state)
           => Searchable state action
           -> Depth
           -> Maybe (Path state action)
dfsLimited p d = depthSearch p d (mkFrontier `addFrontier` is p) E.empty
  where
    is       p = WD (0, Node (initialState p) Nothing 0)

iterativeDeepening :: (Ord state)
                   => Searchable state action
                   -> Maybe (Path state action)
iterativeDeepening p = iterativeDeepeningLimited p NoLimit

iterativeDeepeningLimited :: (Ord state)
                          => Searchable state action
                          -> Depth
                          -> Maybe (Path state action)
iterativeDeepeningLimited p md = go initialDepth p
  where
    go d p
      | isAtDepthLimit d md = dfsLimited p cd
      | otherwise           = dfsLimited p cd <|> go (d+1) p
      where cd = targetDepth d
    initialDepth = 0
