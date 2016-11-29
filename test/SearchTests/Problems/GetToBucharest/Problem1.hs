module SearchTests.Problems.GetToBucharest.Problem1 where

import Control.Search.Internal.Path
import Control.Search.Types
import Data.Word
import SearchTests.Problems.GetToBucharest

initProblem :: GetToBucharest
initProblem =
  Problem _costs
          _heuristic
          _follow
          _goal
          _actions
          Sibiu
  where
    _costs Sibiu Drive Fagaras         = 99
    _costs Sibiu Drive RimnicuVilcea   = 80
    _costs Fagaras Drive Bucharest     = 211
    _costs RimnicuVilcea Drive Pitesti = 97
    _costs Pitesti Drive Bucharest     = 101
    _costs _ _ _                       = error "Invalid Cost"

    _heuristic = sldHeuristic

    _follow Sibiu         Drive = [Fagaras, RimnicuVilcea]
    _follow Fagaras       Drive = [Bucharest]
    _follow RimnicuVilcea Drive = [Pitesti]
    _follow Pitesti       Drive = [Bucharest]
    _follow Bucharest     Drive = []

    _goal Bucharest = True
    _goal _         = False

    _actions Bucharest = []
    _actions _         = [Drive]

bfsAnswer :: Maybe Answer
bfsAnswer = Just $
  Path Bucharest (Just Drive) 211 $
    Path Fagaras (Just Drive) 99 $
      Node Sibiu Nothing 0

dfsAnswer :: Maybe Answer
dfsAnswer = Just $
  Path Bucharest (Just Drive) 101 $
    Path Pitesti (Just Drive) 97 $
      Path RimnicuVilcea (Just Drive) 80 $
        Node Sibiu Nothing 0

dfsLimitedAnswer :: Maybe Answer
dfsLimitedAnswer = bfsAnswer

depthAnswer :: Word32 -> Maybe Answer
depthAnswer 3 = dfsAnswer
depthAnswer 2 = dfsLimitedAnswer
depthAnswer _ = Nothing

ucsAnswer :: Maybe Answer
ucsAnswer = dfsAnswer

greedyAnswer :: Maybe Answer
greedyAnswer = dfsLimitedAnswer

aStarAnswer :: Maybe Answer
aStarAnswer = ucsAnswer
