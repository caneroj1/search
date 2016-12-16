{-# LANGUAGE TupleSections #-}

module Control.Search.Internal.DepthSearch
(
  depthSearch
, isAtGoal
, isAtDepthLimit
) where

import Control.Search.Internal.Depth
import Control.Search.Internal.ExploredSet
import Control.Search.Internal.Path
import Control.Search.Internal.Frontier
import Control.Search.Internal.SearchUtils
import Control.Search.Internal.Stack
import Control.Search.Types
import Data.List
import Data.Maybe
import Data.Word

makeChildren :: Searchable state action
             -> Path state action
             -> Level state action
makeChildren p path = do
  a <- actions p st
  s <- follows p st a
  let nc = costFunction p st a s
    in [Node s (Just a) nc -+- path]
  where
    st = state path

isAtDepthLimit :: Word32 -> Depth -> Bool
isAtDepthLimit _ NoLimit     = False
isAtDepthLimit n (Limited d) = n == d

depthSearch :: (Ord state)
            => Searchable state action
            -> Depth
            -> Frontier Stack state action
            -> ExploredSet state
            -> Maybe (Path state action)
depthSearch p d frontier explored
  | isNothing mbf          = Nothing
  | isExplored st explored = depthSearch p d frontier' explored
  | isAtGoal p path        = Just path
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
