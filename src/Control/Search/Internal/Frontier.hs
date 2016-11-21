{-# LANGUAGE GADTs        #-}

module Control.Search.Internal.Frontier where

import Control.Search.Internal.Container
import Control.Search.Internal.Path
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set hiding (Set)

nullFrontier :: Frontier f a b -> Bool
nullFrontier (Frontier f _) = nullc f

headFrontier :: Frontier f a b -> Maybe (Path a b, Frontier f a b)
headFrontier (Frontier c s)
  | isNothing mbc = Nothing
  | otherwise     = Just (a, Frontier c' s')
  where
    mbc          = pop c
    Just (a, c') = mbc
    s'           = Set.delete (state a) s

addFrontier :: Frontier f a b -> Path a b -> Frontier f a b
addFrontier f@(Frontier c s) pth
  | Set.member st s = f
  | otherwise       = Frontier (insert pth c) (Set.insert st s)
  where st = state pth

mkFrontier :: (Ord a, Container f) => Frontier f a b
mkFrontier = Frontier empty Set.empty

data Frontier f a b where
  Frontier :: (Container f, Ord a) => f (Path a b) -> Set a -> Frontier f a b
