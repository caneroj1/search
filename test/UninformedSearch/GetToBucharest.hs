{-# LANGUAGE TypeFamilies #-}

module UninformedSearch.GetToBucharest where

import Control.Search.Internal.Path
import Control.Search.Types
import Control.Search.BFS
import Data.Set (Set)
import qualified Data.Set as Set hiding (Set)

type Answer = Path Location Drive

data Location = Sibiu
              | Fagaras
              | RimnicuVilcea
              | Pitesti
              | Bucharest
              deriving (Ord, Eq, Show)

data Drive = Drive deriving (Ord, Eq, Show)

data GetToBucharest = Problem {
    costFn   :: Location -> Drive -> Location -> Maybe Integer
  , followFn :: Location -> Drive -> [Location]
  , goalFn   :: Location -> Bool
  , actionFn :: Location -> [Drive]
  , initial  :: Location
  }

instance Searchable GetToBucharest where
  type State  GetToBucharest      = Location
  type Action GetToBucharest      = Drive
  data ExploredSet GetToBucharest = SMap (Set Location)

  isExplored l (SMap s) = Set.member l s
  explore l (SMap a)    = SMap $ Set.insert l a
  emptySet              = SMap Set.empty

  cost s1 a s2 p = costFn p s1 a s2
  follows s a p  = followFn p s a
  goal s p       = goalFn p s
  actions s p    = actionFn p s
  initialState   = initial
