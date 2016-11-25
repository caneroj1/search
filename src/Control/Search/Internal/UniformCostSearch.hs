module Control.Search.Internal.UniformCostSearch
(
  uniformCostSearch
) where

import Control.Search.Internal.Path
import Control.Search.Internal.Weights
import Control.Search.Types
import Data.Maybe

isAtGoal :: (Searchable a) => Path (State a) (Action a) -> a -> Bool
isAtGoal (Node a _ _)   = goal a
isAtGoal (Path a _ _ _) = goal a

uniformCostSearch :: (Searchable a)
                  => a
                  -> Weights (State a) (Action a)
                  -> ExploredSet a
                  -> Maybe (Path (State a) (Action a))
uniformCostSearch p frontier explored
  | isNothing mbf          = Nothing
  | isExplored st explored = uniformCostSearch p frontier' explored
  | isAtGoal path p        = Just path
  | otherwise              = uniformCostSearch p frontier'' explored'
  where
    mbf                     = next frontier
    Just (wpath, frontier') = mbf
    path                    = unwrap wpath
    st                      = state path
    frontier''              = foldr maybeAdd frontier' $! makeChildren p wpath
    explored'               = explore st explored

makeChildren :: (Searchable a)
             => a
             -> WeightedPath (State a) (Action a)
             -> WeightedLevel (State a) (Action a)
makeChildren p wpath = do
  a <- actions st p
  s <- follows st a p
  let nc = cost st a s p
      pc = pathCost wpath
      tc = nc + pc
    in [WP tc (Node s (Just a) nc -+- path)]
  where
    st   = state path
    path = unwrap wpath
