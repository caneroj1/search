{-# LANGUAGE GADTs #-}

module Control.Search.Internal.Frontier
(
  Frontier
, addFrontier
, mkFrontier
, nullf
, headf
) where

import Control.Search.Internal.Queue
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set hiding (Set)

data Frontier a where
  F :: (Ord a) => Queue a -> Set a -> Frontier a

nullf :: Frontier a -> Bool
nullf (F q _) = nullq q

headf :: Frontier a -> Maybe (a, Frontier a)
headf (F q s)
  | isNothing mbq = Nothing
  | otherwise     = Just (a, F q' s')
  where
    mbq          = pop q
    Just (a, q') = mbq
    s'           = Set.delete a s

addFrontier :: Frontier a -> a -> Frontier a
addFrontier f@(F q s) a
  | Set.member a s = f
  | otherwise      = F (insert a q) (Set.insert a s)

mkFrontier :: (Ord a) => Frontier a
mkFrontier = F empty Set.empty
