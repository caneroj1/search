module UninformedSearch.Problem1 where

import Control.Search.Types
import Control.Search.Internal.Path
import Data.Word
import UninformedSearch.GetToBucharest

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

    _heuristic _ = 0

    _follow Sibiu         Drive = [Fagaras, RimnicuVilcea]
    _follow Fagaras       Drive = [Bucharest]
    _follow RimnicuVilcea Drive = [Pitesti]
    _follow Pitesti       Drive = [Bucharest]
    _follow Bucharest     Drive = []

    _goal Bucharest = True
    _goal _         = False

    _actions Bucharest = []
    _actions _         = [Drive]

bfsAnswer :: Answer
bfsAnswer =
  Path Bucharest (Just Drive) 211 $
    Path Fagaras (Just Drive) 99 $
      Node Sibiu Nothing 0

dfsAnswer :: Answer
dfsAnswer =
  Path Bucharest (Just Drive) 101 $
    Path Pitesti (Just Drive) 97 $
      Path RimnicuVilcea (Just Drive) 80 $
        Node Sibiu Nothing 0

dfsLimitedAnswer :: Answer
dfsLimitedAnswer =
  Path Bucharest (Just Drive) 211 $
      Path Fagaras (Just Drive) 99 $
        Node Sibiu Nothing 0

depthAnswer :: Word32 -> Maybe Answer
depthAnswer 3 = Just dfsAnswer
depthAnswer 2 = Just dfsLimitedAnswer
depthAnswer _ = Nothing

ucsAnswer :: Answer
ucsAnswer = dfsAnswer
