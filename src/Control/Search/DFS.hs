module Control.Search.DFS
(
  dfs
) where

import Control.Monad
import Control.Search.Internal.GraphSearch
import Control.Search.Internal.Path
import Control.Search.Internal.Frontier
import Control.Search.Internal.Stack
import Control.Search.Types
import Data.List
import Data.Maybe

dfs :: (Searchable a) => a -> Maybe (Path (State a) (Action a))
dfs p = graphSearch p (frontier p) emptySet
  where
    is       p = Node (initialState p) Nothing Nothing
    frontier :: (Searchable a) => a -> Frontier Stack (State a) (Action a)
    frontier p = mkFrontier `addFrontier` is p
