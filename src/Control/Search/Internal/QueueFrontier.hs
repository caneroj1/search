{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Search.Internal.QueueFrontier
(
  QueueFrontier
) where

import Control.Search.Internal.Frontier
import Control.Search.Internal.Path
import Control.Search.Internal.Queue
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set hiding (Set)

nullf :: QueueFrontier a b -> Bool
nullf (F q _) = nullq q

headf :: QueueFrontier a b -> Maybe (Path a b, QueueFrontier a b)
headf (F q s)
  | isNothing mbq = Nothing
  | otherwise     = Just (a, F q' s')
  where
    mbq          = pop q
    Just (a, q') = mbq
    s'           = Set.delete (state a) s

addf :: QueueFrontier a b -> Path a b -> QueueFrontier a b
addf f@(F q s) pth
  | Set.member st s = f
  | otherwise       = F (insert pth q) (Set.insert st s)
  where st = state pth

mkf :: (Ord a) => QueueFrontier a b
mkf = F empty Set.empty

data QueueFrontier a b where
  F :: (Ord a) => Queue (Path a b) -> Set a -> QueueFrontier a b

instance (Ord a) => FrontierSearch (QueueFrontier a b) where
  type FrontierElem (QueueFrontier a b) = Path a b
  nullFrontier = nullf
  headFrontier = headf
  addFrontier  = addf
  makeFrontier = mkf
