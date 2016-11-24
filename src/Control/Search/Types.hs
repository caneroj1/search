{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Search.Types
(
  Cost
, Searchable(..)
) where

import Control.Search.Internal.Path (Cost)

class (Ord (State a), Ord (Action a)) => Searchable a where
  -- | Associated type that represents the type of States in the problem space.
  type State a

  -- | Associated type that represents the actions that can be performed in the
  -- problem space.
  type Action a

  -- | Associated data type that determines which data structure to use
  -- to efficiently check if a state in the problem space has already been
  -- explored.
  data ExploredSet a :: *

  -- | isExplored is a function to determine whether a state has been explored.
  isExplored :: State a -> ExploredSet a -> Bool

  -- | explore is a function that should add the given state to the ExploredSet.
  explore :: State a -> ExploredSet a -> ExploredSet a

  -- | function to make an empty ExploredSet.
  emptySet :: ExploredSet a

  -- | cost is a function that should return, given the current state, an action
  -- to be performed on that state, and the state that is the result of that
  -- action, the cost of doing that action.
  cost :: State a -> Action a -> State a -> a -> Cost

  -- | follows is a function that, given a state and an action to be performed
  -- on that state, returns a new list of states.
  follows :: State a -> Action a -> a -> [State a]

  -- | goal is a function should tell you whether or not the current state
  -- is a goal state
  goal :: State a -> a -> Bool

  -- | actions is a function that maps each state to the list of actions
  -- that can be performed on that state.
  actions :: State a -> a -> [Action a]

  -- | the starting state of the problem.
  initialState :: a -> State a
