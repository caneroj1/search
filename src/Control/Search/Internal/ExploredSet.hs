module Control.Search.Internal.ExploredSet where

import qualified Data.Set as S

newtype ExploredSet state = ExploredSet {
    unEx :: S.Set state
  }

explore :: (Ord state) => state -> ExploredSet state -> ExploredSet state
explore s = ExploredSet . S.insert s . unEx

isExplored :: (Ord state) => state -> ExploredSet state -> Bool
isExplored s = S.member s . unEx

empty :: ExploredSet state
empty = ExploredSet S.empty
