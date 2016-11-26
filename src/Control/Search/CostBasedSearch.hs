module Control.Search.CostBasedSearch
(
  uniform
, greedy
, astar
) where

import Control.Search.Internal.ExploredSet
import Control.Search.Internal.Path
import Control.Search.Internal.UniformCostSearch
import Control.Search.Internal.Weights
import Control.Search.Types

uniform :: (Ord state, Ord action)
        => Searchable state action
        -> Maybe (Path state action)
uniform p = costBasedSearch p CostOnly

greedy :: (Ord state, Ord action)
        => Searchable state action
        -> Maybe (Path state action)
greedy p = costBasedSearch p HeuristicOnly

astar :: (Ord state, Ord action)
        => Searchable state action
        -> Maybe (Path state action)
astar p = costBasedSearch p AStar

costBasedSearch :: (Ord state, Ord action)
                => Searchable state action
                -> CostSettings
                -> Maybe (Path state action)
costBasedSearch p settings = uniformCostSearch p settings (frontier p) empty
  where
    is       p = WP 0 0 $ Node (initialState p) Nothing 0
    frontier p = maybeAdd (is p) mkWeights
