module Control.Search.Internal.UniformCostSearch
(
  uniformCostSearch
) where

import Control.Search.Internal.ExploredSet
import Control.Search.Internal.Path
import Control.Search.Internal.Weights
import Control.Search.Types
import Data.Maybe

isAtGoal :: Searchable state action -> Path state action -> Bool
isAtGoal s (Node a _ _)   = goal s a
isAtGoal s (Path a _ _ _) = goal s a

uniformCostSearch :: (Ord state, Ord action)
                  => Searchable state action
                  -> Weights state action
                  -> ExploredSet state
                  -> Maybe (Path state action)
uniformCostSearch p frontier explored
  | isNothing mbf          = Nothing
  | isExplored st explored = uniformCostSearch p frontier' explored
  | isAtGoal p path        = Just path
  | otherwise              = uniformCostSearch p frontier'' explored'
  where
    mbf                     = next frontier
    Just (wpath, frontier') = mbf
    path                    = unwrap wpath
    st                      = state path
    frontier''              = foldr maybeAdd frontier' $! makeChildren p wpath
    explored'               = explore st explored

makeChildren :: Searchable state action
             -> WeightedPath state action
             -> WeightedLevel state action
makeChildren p wpath = do
  a <- actions p st
  s <- follows p st a
  let nc = costFunction p st a s
      pc = pathCost wpath
      tc = nc + pc
    in [WP tc (Node s (Just a) nc -+- path)]
  where
    st   = state path
    path = unwrap wpath
