{-# LANGUAGE TypeFamilies #-}

module Control.Search.Internal.Frontier
(
  FrontierSearch(..)
) where

import Control.Search.Internal.Path

class FrontierSearch a where
  type FrontierElem a
  nullFrontier :: a -> Bool
  headFrontier :: a -> Maybe (FrontierElem a, a)
  addFrontier  :: a -> FrontierElem a -> a
  makeFrontier :: a
