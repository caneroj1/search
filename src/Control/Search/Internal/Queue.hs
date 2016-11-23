{-# LANGUAGE TypeFamilies #-}

module Control.Search.Internal.Queue
(
  Queue
) where

import Control.Search.Internal.Container
import Data.Sequence (Seq, ViewL((:<)), (|>))
import qualified Data.Sequence as Seq hiding (Seq)

newtype Queue a = Q {
    unQueue :: Seq (ContainerElem a)
  }

qempty :: Queue a
qempty = Q Seq.empty

qinsert :: ContainerElem a -> Queue a -> Queue a
qinsert a = Q . flip (|>) a . unQueue

qnull :: Queue a -> Bool
qnull = Seq.null . unQueue

qpop :: Queue a -> Maybe (ContainerElem a, Queue a)
qpop = _pop . Seq.viewl . unQueue
  where
    _pop (h :< s) = Just (h, Q s)
    _pop _        = Nothing

instance Container Queue where
  empty  = qempty
  insert = qinsert
  nullc  = qnull
  pop    = qpop
