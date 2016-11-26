{-# LANGUAGE GADTs #-}

module Control.Search.Types
(
  Cost
, Searchable(..)
, CostSettings(..)
, costFunction
, heuristic
, follows
, goal
, actions
, initialState
) where

import Control.Search.Internal.Path (Cost)

type CostFunction state action = state -> action -> state -> Cost
type Heuristic state = state -> Cost
type Follows state action = state -> action -> [state]
type Goal state = state -> Bool
type Actions state action = state -> [action]

data Searchable state action where
  Problem :: (Ord state, Ord action)
          => CostFunction state action
          -> Heuristic state
          -> Follows state action
          -> Goal state
          -> Actions state action
          -> state
          -> Searchable state action

data CostSettings = CostOnly
                  | HeuristicOnly
                  | AStar

costFunction :: Searchable state action -> CostFunction state action
costFunction (Problem c _ _ _ _ _) = c

heuristic :: Searchable state action -> Heuristic state
heuristic (Problem _ h _ _ _ _) = h

follows :: Searchable state action -> Follows state action
follows (Problem _ _ f _ _ _) = f

goal :: Searchable state action -> Goal state
goal (Problem _ _ _ g _ _) = g

actions :: Searchable state action -> Actions state action
actions (Problem _ _ _ _ a _) = a

initialState :: Searchable state action -> state
initialState (Problem _ _ _ _ _ i) = i
