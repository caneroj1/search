module Control.Search.PathFunctions
(
  pathCosts
, totalCost
, pathActions
, pathStates
, actionsAtEachState
, actionsAndStatesWithCosts
) where

import Control.Search.Internal.Path

-- | Returns the costs accrued at each step of the solution
--  path. The first element is the initial state and has a cost of 0.
pathCosts :: Path state action -> [Cost]
pathCosts (Node _ _ c)   = [c]
pathCosts (Path _ _ c p) = pathCosts p ++ [c]

-- | Returns the total cost of the solution path found.
totalCost :: Path state action -> Cost
totalCost = sum . pathCosts

-- | Returns the sequence of potential actions taken
--  along the solution path.
pathActions :: Path state action -> [Maybe action]
pathActions (Node _ ma _)   = [ma]
pathActions (Path _ ma _ p) = pathActions p ++ [ma]

-- | Returns the sequence of states entered along
--  the solution path.
pathStates :: Path state action -> [state]
pathStates (Node s _ _)   = [s]
pathStates (Path s _ _ p) = pathStates p ++ [s]

-- | Returns a list of the actions taken at each state
--  along the solution path.
actionsAtEachState :: Path state action -> [(state, Maybe action)]
actionsAtEachState p = zip (pathStates p) (pathActions p)

-- | Returns a list of the states, actions, and costs along the solution
--  path. Each element of the list is a tuple that contains the state,
--  the action taken from that state, and the cost to take that action.
actionsAndStatesWithCosts :: Path state action -> [(state, Maybe action, Cost)]
actionsAndStatesWithCosts p = zip3 (pathStates p)
                                   (pathActions p)
                                   (drop 1 $ pathCosts p ++ [0])
