module Control.Search.UCS
(
  ucs
) where

import Control.Search.Internal.ExploredSet
import Control.Search.Internal.Path
import Control.Search.Internal.UniformCostSearch
import Control.Search.Internal.Weights
import Control.Search.Types

ucs :: (Ord state, Ord action)
    => Searchable state action
    -> Maybe (Path state action)
ucs p = uniformCostSearch p (frontier p) empty
  where
    is       p = WP 0 $ Node (initialState p) Nothing 0
    frontier p = maybeAdd (is p) mkWeights
