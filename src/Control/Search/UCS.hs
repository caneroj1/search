module Control.Search.UCS
(
  ucs
) where

import Control.Search.Internal.Path
import Control.Search.Internal.UniformCostSearch
import Control.Search.Internal.Weights
import Control.Search.Types

ucs :: (Searchable a) => a -> Maybe (Path (State a) (Action a))
ucs p = uniformCostSearch p (frontier p) emptySet
  where
    is       p = WP 0 $ Node (initialState p) Nothing 0
    frontier :: (Searchable a) => a -> Weights (State a) (Action a)
    frontier p = maybeAdd (is p) mkWeights
