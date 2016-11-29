module UnitTests.PathFunctionsUnitTests
(
  unitTests
) where

import qualified Control.Search.Internal.Path as P
import qualified Control.Search
import qualified SearchTests.Problems.GetToBucharest as B
import Test.HUnit.Base

unitTests :: [Test]
unitTests = [
              TestCase testPathCosts
            , TestCase testTotalCost
            , TestCase testPathActions
            , TestCase testPathStates
            , TestCase testActionsAtEachState
            , TestCase testActionsAndStatesWithCosts
            ]

basePath :: P.Path B.Location B.Drive
basePath =
  P.Path B.Bucharest (Just B.Drive) 101 $
    P.Path B.Pitesti (Just B.Drive) 97 $
      P.Path B.RimnicuVilcea (Just B.Drive) 80 $
        P.Node B.Sibiu Nothing 0

testPathCosts :: Assertion
testPathCosts =
  assertEqual "Path costs not correct"
  [0, 80, 97, 101] (Control.Search.pathCosts basePath)

testTotalCost :: Assertion
testTotalCost =
  assertEqual "Total cost not correct"
    278 (Control.Search.totalCost basePath)

testPathActions :: Assertion
testPathActions =
  assertEqual "Path actions not correct"
  [Nothing, Just B.Drive, Just B.Drive, Just B.Drive]
  (Control.Search.pathActions basePath)

testPathStates :: Assertion
testPathStates =
  assertEqual "Path states not correct"
  [B.Sibiu, B.RimnicuVilcea, B.Pitesti, B.Bucharest]
  (Control.Search.pathStates basePath)

testActionsAtEachState :: Assertion
testActionsAtEachState =
  assertEqual "List of actions at each state not correct"
  [
    (B.Sibiu, Nothing)
  , (B.RimnicuVilcea, Just B.Drive)
  , (B.Pitesti, Just B.Drive)
  , (B.Bucharest, Just B.Drive)
  ]
  (Control.Search.actionsAtEachState basePath)

testActionsAndStatesWithCosts :: Assertion
testActionsAndStatesWithCosts =
  assertEqual "List of actions at each state w/ costs not correct"
  [
    (B.Sibiu, Nothing, 80)
  , (B.RimnicuVilcea, Just B.Drive, 97)
  , (B.Pitesti, Just B.Drive, 101)
  , (B.Bucharest, Just B.Drive, 0)
  ]
  (Control.Search.actionsAndStatesWithCosts basePath)
