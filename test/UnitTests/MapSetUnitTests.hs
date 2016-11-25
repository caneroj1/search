module UnitTests.MapSetUnitTests
(
  unitTests
) where

import qualified Control.Search.Internal.MapSet as MS
import Test.HUnit.Base

unitTests :: [Test]
unitTests = TestCase canAddToEmptyMapSet                  :
            TestCase canAddMultipleItemsWithTheSameWeight :
            TestCase canAddItemWithAnotherKey             :
            TestCase canDeleteItemFromMapSet              :
            minViewTests

mapSet :: MS.MapSet Int Char
mapSet = MS.empty

baseMapSet = MS.insert 10 'c' mapSet
nextMapSet = MS.insert 10 'd' (MS.insert 10 'e' baseMapSet)
mapSet3    = MS.insert 7  'a' nextMapSet

canAddToEmptyMapSet :: Assertion
canAddToEmptyMapSet =
  assertEqual "Could not add (W=10, V='c') to empty MapSet"
    [(10, ['c'])]
    (MS.elems baseMapSet)

canAddMultipleItemsWithTheSameWeight :: Assertion
canAddMultipleItemsWithTheSameWeight =
  assertEqual "Could not add other items to base MapSet"
    [(10, ['c', 'd', 'e'])]
    (MS.elems nextMapSet)

canAddItemWithAnotherKey :: Assertion
canAddItemWithAnotherKey =
  assertEqual "Could not add (W=7, V='a') to MapSet"
  [(7, ['a']), (10, ['c', 'd', 'e'])]
  (MS.elems mapSet3)

canDeleteItemFromMapSet :: Assertion
canDeleteItemFromMapSet =
  assertEqual "Could not delete item from MapSet"
    [(10, ['c', 'e'])]
    (MS.elems $ MS.delete 10 'd' nextMapSet)

minViewTests :: [Test]
minViewTests =
  [
    TestCase minViewIsCorrectWithOneKey
  , TestCase minViewIsCorrectWithTwoKeys
  , TestCase minViewIsNothingIfEmpty
  ]

minViewIsCorrectWithTwoKeys :: Assertion
minViewIsCorrectWithTwoKeys =
  assertBool "Could not get correct minView from MapSet with 2 keys" $
    minElem  == 'a' &&
    mapElems == [(10, ['c', 'd', 'e'])]
  where
    Just (minElem, ms) = MS.minView mapSet3
    mapElems           = MS.elems ms

minViewIsCorrectWithOneKey :: Assertion
minViewIsCorrectWithOneKey =
  assertBool "Could not get correct minView from MapSet with 1 key" $
    minElem  == 'c' &&
    mapElems == [(10, ['d', 'e'])]
  where
    Just (minElem, ms) = MS.minView nextMapSet
    mapElems           = MS.elems ms

minViewIsNothingIfEmpty :: Assertion
minViewIsNothingIfEmpty =
  assertEqual "minView is Nothing when empty" Nothing (MS.minView mapSet)
