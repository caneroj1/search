{-# LANGUAGE TupleSections #-}

module Control.Search.Internal.DepthSearch
(
  depthSearch
, isAtGoal
, isAtDepthLimit
) where

import Control.Monad
import Control.Search.Internal.Depth
import Control.Search.Internal.Path
import Control.Search.Internal.Frontier
import Control.Search.Internal.Stack
import Control.Search.Types
import Data.List
import Data.Maybe
import Data.Word

isAtGoal :: (Searchable a) => Path (State a) (Action a) -> a -> Bool
isAtGoal (Node a _ _)   = goal a
isAtGoal (Path a _ _ _) = goal a

-- move this stuff to general graph search utils?
-- graphSearch :: (Searchable a, Container c)
--             => a
--             -> Depth
--             -> Frontier c (State a) (Action a)
--             -> ExploredSet a
--             -> Maybe (Path (State a) (Action a))
-- graphSearch p depth frontier explored
--   | isNothing mbf          = Nothing
--   | isExplored st explored = graphSearch p frontier' explored
--   | isAtGoal path p        = Just path
--   | isAtDepthLimit d depth = graphSearch p frontier'  explored'
--   | otherwise              = graphSearch p frontier'' explored'
--   where
--     d                      =
--     mbf                    = headFrontier frontier
--     Just (path, frontier') = mbf
--     st                     = state path
--     frontier''             = foldl' addFrontier frontier' $! makeChildren p path
--     explored'              = explore st explored

makeChildren :: (Searchable a)
             => a
             -> Path (State a) (Action a)
             -> Level (State a) (Action a)
makeChildren p path = do
  a <- actions st p
  s <- follows st a p
  let nc = cost st a s p
    in [Node s (Just a) nc -+- path]
  where
    st = state path

isAtDepthLimit :: Word32 -> Depth -> Bool
isAtDepthLimit _ NoLimit     = False
isAtDepthLimit n (Limited d) = n == d

depthSearch :: (Searchable a)
            => a
            -> Depth
            -> Frontier Stack (State a) (Action a)
            -> ExploredSet a
            -> Maybe (Path (State a) (Action a))
depthSearch p d frontier explored
  | isNothing mbf          = Nothing
  | isExplored st explored = depthSearch p d frontier' explored
  | isAtGoal path p        = Just path
  | isAtDepthLimit cd d    = depthSearch p d frontier' explored'
  | otherwise              = depthSearch p d frontier'' explored'
  where
    mbf                  = headFrontier frontier
    Just (pd, frontier') = mbf
    (cd, path)           = unWD pd
    st                   = state path
    frontier''           = foldl' addFrontier frontier'
                            . map (WD . (cd + 1,))
                              $! makeChildren p path
    explored'            = explore st explored
