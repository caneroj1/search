module Control.Search.Internal.Queue
(
  Queue
, insert
, empty
, nullq
, pop
) where

import Data.Sequence (Seq, ViewL((:<)), (|>))
import qualified Data.Sequence as Seq hiding (Seq)

newtype Queue a = Q {
    unQueue :: Seq a
  }

empty :: Queue a
empty = Q Seq.empty

insert :: a -> Queue a -> Queue a
insert a = Q . flip (|>) a . unQueue

nullq :: Queue a -> Bool
nullq = Seq.null . unQueue

pop :: Queue a -> Maybe (a, Queue a)
pop = _pop . Seq.viewl . unQueue
  where
    _pop (h :< s) = Just (h, Q s)
    _pop _        = Nothing
