{-# LANGUAGE RecordWildCards #-}

module Control.Search.Internal.Weights
(
  Weights(..)
, mkWeights
, next
, maybeAdd
) where

import           Control.Search.Internal.MapSet (MapSet)
import qualified Control.Search.Internal.MapSet as MS hiding (MapSet)
import           Control.Search.Internal.Path
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M hiding (Map)
import           Data.Maybe

mkWeights :: (Ord a, Ord b) => Weights a b
mkWeights = Weights MS.empty M.empty

data Weights a b = Weights {
    weightToVMapSet :: MapSet Double (WeightedPath a b)
  , vToWeightMap    :: Map (WeightedPath a b) Double
  }

next :: (Ord a, Ord b) => Weights a b -> Maybe (WeightedPath a b, Weights a b)
next Weights{..} =
  updateWeights <$> MS.minView weightToVMapSet
  where
    updateWeights (mn, weightsSet) =
      (mn, Weights weightsSet $ M.delete mn vToWeightMap)

maybeAdd :: (Ord a, Ord b) => WeightedPath a b -> Weights a b -> Weights a b
maybeAdd w@(WP c _ _) ws@Weights{..}
  | isNothing mw =
    Weights (MS.insert c w weightToVMapSet)
            (M.insert w c vToWeightMap)
  | c >= prev    = ws
  | otherwise    =
    Weights (MS.insert c w $ MS.delete prev w weightToVMapSet)
            (M.insert w c $ M.delete w vToWeightMap)
  where
    mw        = M.lookup w vToWeightMap
    Just prev = mw
