{-# LANGUAGE RecordWildCards #-}

module Control.Search.Internal.Weights
(
  Weights
, mkWeights
, next
-- , maybeAdd
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
    weightToVMapSet :: MapSet Double (Path a b)
  , vToWeightMap    :: Map (Path a b) Double
  }

next :: (Ord a, Ord b) => Weights a b -> Maybe (Path a b, Weights a b)
next Weights{..} =
  updateWeights <$> MS.minView weightToVMapSet
  where
    updateWeights (mn, weightsSet) =
      (mn, Weights weightsSet $ M.delete mn vToWeightMap)

-- maybeAdd :: (Ord a, Ord b) => Path a b -> Weights a b -> Weights a b
-- maybeAdd w v ws@Weights{..}
--   | isNothing mw =
--     Weights (insert w v weightToVMapSet)
--             (M.insert v w vToWeightMap)
--   | w >= prev    = ws
--   | otherwise    =
--     Weights (insert w v $ delete prev v weightToVMapSet)
--             (M.update (\_ -> Just w) v vToWeightMap)
--   where
--     mw        = M.lookup v vToWeightMap
--     Just prev = mw
