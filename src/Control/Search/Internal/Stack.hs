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

data Stack a = Stack Word32 (WithDepth a) (Stack a)
             | Empty

sempty :: Stack a
sempty = Empty

sinsert :: WithDepth a -> Stack a -> Stack a
sinsert a s@(Stack d ls _) = Stack (d + 1) a s
sinsert a Empty            = Stack 0 a Empty

snull :: Stack a -> Bool
snull Empty   = True
snull Stack{} = False

spop :: Stack a -> Maybe (WithDepth a, Stack a)
spop (Stack _ a s) = Just (a, s)
spop Empty         = Nothing

instance Container Stack where
  type Elem Stack = WithDepth
  empty  = sempty
  insert = sinsert
  nullc  = snull
  pop    = spop

instance Underlying WithDepth where
  underly = snd . unWD
