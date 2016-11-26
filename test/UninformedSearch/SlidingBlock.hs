{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module UninformedSearch.SlidingBlock where

import Control.Lens
import Control.Search
import Data.Array
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM hiding (HashMap)
import GHC.Generics

data Tile = T1 | T2 | T3 | T4
          | T5 | T6 | T7 | T8
          | E
          deriving (Eq, Ord, Show, Generic)

instance Hashable Tile

data Swap = Swap Int deriving (Eq, Ord, Show)

data Blocks = Puzzle {
    positionOfBlank :: Int
  , board           :: Array Int Tile
  } deriving (Eq, Ord, Show)

winningState :: [Tile]
winningState =
  [
      E,  T1, T2
    , T3, T4, T5
    , T6, T7, T8
  ]

properPositions :: HashMap Tile Int
properPositions = HM.fromList $ zip winningState [1..]

goal :: Blocks -> Bool
goal p = elems (board p) == winningState

arraySwap :: (Ix i) => Array i e -> i -> i -> Array i e
arraySwap arr i j =
  arr & ix i .~ (arr ^?! ix j) & ix j .~ (arr ^?! ix i)

computeNextBoardState :: Blocks -> Swap -> [Blocks]
computeNextBoardState Puzzle{..} (Swap j) =
  let i      = positionOfBlank
      board' = arraySwap board i j
      blank' = j
    in [Puzzle blank' board']
  where
    i = positionOfBlank

-- randomize this?
initialState :: Blocks
initialState = Puzzle {
    positionOfBlank = 5
  , board           = listArray (1, 9) initialBoard
  }
  where
    initialBoard =
      [
          T7, T2, T4
        , T5, E,  T6
        , T8, T3, T1
      ]

-- heuristic function will count the number of tileso ut of place
heuristic :: Blocks -> Cost
heuristic Puzzle{..} = sum . map isOutOfPlace $ assocs board
  where
    isOutOfPlace (ix, e)
      | truePosition == ix = 0
      | otherwise          = 1
      where
        truePosition = HM.lookupDefault 0 e properPositions

type SlidingBlockPuzzle = Searchable Blocks Swap

initProblem :: SlidingBlockPuzzle
initProblem = Problem _costs
                      _heuristic
                      _follow
                      _goal
                      _actions
                      initialState
  where
    _costs _ _ _ = 1
    _heuristic   = heuristic
    _follow      = computeNextBoardState
    _goal        = goal
    _actions     = actionsWithBlank . positionOfBlank
      where
        actionsWithBlank 1 = [Swap 2, Swap 4]
        actionsWithBlank 2 = [Swap 1, Swap 3, Swap 5]
        actionsWithBlank 3 = [Swap 2, Swap 6]
        actionsWithBlank 4 = [Swap 1, Swap 4, Swap 7]
        actionsWithBlank 5 = [Swap 2, Swap 4, Swap 6, Swap 8]
        actionsWithBlank 6 = [Swap 3, Swap 5, Swap 9]
        actionsWithBlank 7 = [Swap 4, Swap 8]
        actionsWithBlank 8 = [Swap 5, Swap 7, Swap 9]
        actionsWithBlank 9 = [Swap 6, Swap 8]
