{-# LANGUAGE RecordWildCards #-}

module UnitTests.WeightsUnitTests
(
  unitTests
) where

import qualified Data.Map.Strict as M (toList)
import qualified Control.Search.Internal.MapSet as MS (toList)
import Control.Search.Internal.Path
import Control.Search.Internal.Weights
import Test.HUnit.Base

unitTests :: [Test]
unitTests = TestCase canAddToEmptyWeights             :
            TestCase canReplaceWithBetterWeight       :
            TestCase canAddOtherElementWithSameWeight :
            canGetGetNextFromWeights

toWP :: Cost -> Char -> Char -> WeightedPath Char Char
toWP cost c1 c2 = WP cost (Node c1 (Just c2) cost)

emptyWeights :: Weights Char Char
emptyWeights = mkWeights

firstElem :: WeightedPath Char Char
firstElem = toWP 15 'a' 'a'

newElem :: WeightedPath Char Char
newElem = toWP 15 'c' 'a'

betterElem :: WeightedPath Char Char
betterElem = toWP 10 'a' 'a'

newElem2 :: WeightedPath Char Char
newElem2 = toWP 1 'd' 'a'

weights1 :: Weights Char Char
weights1 = firstElem `maybeAdd` emptyWeights

weights2 :: Weights Char Char
weights2 = newElem `maybeAdd` weights1

weights3 :: Weights Char Char
weights3 = newElem2 `maybeAdd` weights2

weightElems :: Weights a b -> [(Double, [WeightedPath a b])]
weightElems Weights{..} = MS.toList weightToVMapSet

mapElems :: Weights a b -> [(WeightedPath a b, Double)]
mapElems Weights{..} = M.toList vToWeightMap

canAddToEmptyWeights :: Assertion
canAddToEmptyWeights =
  assertBool "Could not add element with W=10 to weights" $
    melems == [(firstElem, 15)]   &&
    welems == [(15, [firstElem])]
  where
    melems   = mapElems weights1
    welems   = weightElems weights1

canGetGetNextFromWeights :: [Test]
canGetGetNextFromWeights =
  [
    TestCase nextElementWithOneInWeights
  , TestCase nextElementWithTwoInWeights
  , TestCase nextElementWithTwoDiffWeights
  ]

nextElementWithOneInWeights :: Assertion
nextElementWithOneInWeights =
  assertBool "Could not get next element from weights1" $
    p == firstElem &&
    null melems    &&
    null welems
  where
    Just (p, ws) = next weights1
    melems       = mapElems ws
    welems       = weightElems ws

nextElementWithTwoInWeights :: Assertion
nextElementWithTwoInWeights =
  assertBool "Could not get next element from weights2" $
    p == firstElem              &&
    melems == [(newElem, 15)]   &&
    welems == [(15, [newElem])]
  where
    Just (p, ws) = next weights2
    melems       = mapElems ws
    welems       = weightElems ws

nextElementWithTwoDiffWeights :: Assertion
nextElementWithTwoDiffWeights =
  assertBool "Could not get next element from weights3" $
    p == newElem2                              &&
    melems == [(firstElem, 15), (newElem, 15)] &&
    welems == [(15, [firstElem, newElem])]
  where
    Just (p, ws) = next weights3
    melems       = mapElems ws
    welems       = weightElems ws

canReplaceWithBetterWeight :: Assertion
canReplaceWithBetterWeight =
  assertBool "Could not replace element with a better weight" $
    melems == [(betterElem, 10)]    &&
    welems == [(10, [betterElem])]
  where
    weights2 = betterElem `maybeAdd` weights1
    melems   = mapElems weights2
    welems   = weightElems weights2

canAddOtherElementWithSameWeight :: Assertion
canAddOtherElementWithSameWeight =
  assertBool "Could not add new element with same weight" $
    melems == [(firstElem, 15), (newElem, 15)] &&
    welems == [(15, [firstElem, newElem])]
  where
    melems   = mapElems weights2
    welems   = weightElems weights2
