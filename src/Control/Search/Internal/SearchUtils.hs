module Control.Search.Internal.SearchUtils where

import Control.Search.Types
import Control.Search.Internal.Path

isAtGoal :: Searchable state action -> Path state action -> Bool
isAtGoal s (Node a _ _)   = goal s a
isAtGoal s (Path a _ _ _) = goal s a
