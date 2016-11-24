{-# LANGUAGE TupleSections #-}

module Control.Search.Internal.MapSet
(
  MapSet
, empty
, minView
, insert
, delete
) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M hiding (Map)
import           Data.Maybe
import           Data.Set        (Set)
import qualified Data.Set        as S hiding (Set)

newtype MapSet k v = MapSet {
    unMapSet :: Map k (Set v)
  }

empty :: (Ord k, Ord v) => MapSet k v
empty = MapSet M.empty

minView :: (Ord k, Ord v) => MapSet k v -> Maybe (v, MapSet k v)
minView (MapSet m) =
  updateMapWithSet <$> (getMinFromSet =<< M.minViewWithKey m)
  where
    getMinFromSet ((k, s), m')       = uncurry (k,m',,) <$> S.minView s
    updateMapWithSet (k, m', mn, s') =
      (mn, MapSet $ M.update (\_ -> return s') k m')

insert :: (Ord k, Ord v) => k -> v -> MapSet k v -> MapSet k v
insert k v = MapSet . M.update (return . S.insert v) k . unMapSet

delete :: (Ord k, Ord v) => k -> v -> MapSet k v -> MapSet k v
delete k v = MapSet . M.update mkNewSet k . unMapSet
  where
    mkNewSet s
      | S.null s  = Nothing
      | otherwise = Just s'
      where s' = S.delete v s
