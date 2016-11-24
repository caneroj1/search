{-# LANGUAGE TupleSections #-}

module Control.Search.Internal.UniformCostSearch
(

) where

import Control.Monad
import Control.Search.Internal.Path
import Control.Search.Internal.Weights
import Control.Search.Types
import Data.List
import Data.Maybe
import Data.Word
--
-- isAtGoal :: (Searchable a) => Path (State a) (Action a) -> a -> Bool
-- isAtGoal (Node a _ _)   = goal a
-- isAtGoal (Path a _ _ _) = goal a
--
-- uniformCostSearch :: (Searchable a)
--                   => a
--                   -> Weights (Path (State a) (Action a))
--                   -> ExploredSet a
--                   -> Maybe (Path (State a) (Action a))
-- uniformCostSearch p frontier explored
--   | isNothing mbf          = Nothing
--   | isExplored st explored = graphSearch p frontier' explored
--   | isAtGoal path p        = Just path
--   | otherwise              = graphSearch p frontier'' explored'
--   where
--     mbf                    = next frontier
--     Just (path, frontier') = mbf
--     st                     = state path
--     frontier''             = foldl' maybeAdd frontier' $! makeChildren p path
--     explored'              = explore st explored
--     addWithWeight (wt, c)  = maybeAdd wt c
--
-- makeChildren :: (Searchable a)
--              => a
--              -> Path (State a) (Action a)
--              -> Level (State a) (Action a)
-- makeChildren p path = do
--   a <- actions st p
--   s <- follows st a p
--   let nc = cost st a s p
--     in [Node s (Just a) nc -+- path]
--   where
--     st = state path
