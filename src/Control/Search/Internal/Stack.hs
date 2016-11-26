{-# LANGUAGE TypeFamilies #-}

module Control.Search.Internal.Stack
(
  Stack
, WithDepth(..)
)
where

import Control.Search.Internal.Container
import Control.Search.Internal.Depth
import Data.Word

newtype WithDepth a = WD {
    unWD :: (Word32, a)
  }

data Stack a = Stack (WithDepth a) (Stack a)
             | Empty

sempty :: Stack a
sempty = Empty

sinsert :: WithDepth a -> Stack a -> Stack a
sinsert a s@(Stack ls _) = Stack a s
sinsert a Empty          = Stack a Empty

snull :: Stack a -> Bool
snull Empty   = True
snull Stack{} = False

spop :: Stack a -> Maybe (WithDepth a, Stack a)
spop (Stack a s) = Just (a, s)
spop Empty       = Nothing

instance Container Stack where
  type Elem Stack = WithDepth
  empty  = sempty
  insert = sinsert
  nullc  = snull
  pop    = spop

instance Underlying WithDepth where
  underly = snd . unWD
