module UninformedSearch.Problem2 where

import Control.Search.Internal.Path
import UninformedSearch.GetToBucharest

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
    _costs Fagaras Drive RimnicuVilcea = Just 10
    _costs Fagaras Drive Pitesti       = Just 40
    _costs RimnicuVilcea Drive Pitesti = Just 97
    _costs RimnicuVilcea Drive Fagaras = Just 10
    _costs Pitesti Drive Bucharest     = Just 101
    _costs _ _ _                       = Nothing

    _follow Sibiu         Drive = [Fagaras,       RimnicuVilcea]
    _follow Fagaras       Drive = [RimnicuVilcea, Pitesti]
    _follow RimnicuVilcea Drive = [Pitesti,       Fagaras]
    _follow Pitesti       Drive = [Bucharest]
    _follow Bucharest     Drive = []

    _goal Bucharest = True
    _goal _         = False

    _actions Bucharest = []
    _actions _         = [Drive]

answer :: Answer
answer =
  Path Bucharest (Just Drive) (Just 101) $
    Path Pitesti (Just Drive) (Just 40)  $
      Path Fagaras (Just Drive) (Just 99)  $
        Node Sibiu Nothing Nothing