{-# LANGUAGE TypeFamilies #-}

module UninformedSearch.Problem where

import Control.Search.Types
import Control.Search.BFS
import Data.Set (Set)
import qualified Data.Set as Set hiding (Set)

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

initProblem :: GetToBucharest
initProblem = Problem {
    costFn   = _costs
  , followFn = _follow
  , goalFn   = _goal
  , actionFn = _actions
  , initial  = Sibiu
  }
  where
    _costs Sibiu Drive Fagaras         = Just 99
    _costs Sibiu Drive RimnicuVilcea   = Just 80
    _costs Fagaras Drive Bucharest     = Just 211
    _costs RimnicuVilcea Drive Pitesti = Just 97
    _costs Pitesti Drive Bucharest     = Just 101
    _costs _ _ _                       = Nothing

    _follow Sibiu         Drive = [Fagaras, RimnicuVilcea]
    _follow Fagaras       Drive = [Bucharest]
    _follow RimnicuVilcea Drive = [Pitesti]
    _follow Pitesti       Drive = [Bucharest]
    _follow Bucharest     Drive = []

    _goal Bucharest = True
    _goal _         = False

    _actions Bucharest = []
    _actions _         = [Drive]


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
