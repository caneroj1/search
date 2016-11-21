module Control.Search.Internal.Stack
(
  Stack
, depth
)
where

import Control.Search.Internal.Container

data Stack a = Stack Int [a]

sempty :: Stack a
sempty = Stack 0 []

sinsert :: a -> Stack a -> Stack a
sinsert a (Stack d ls) = Stack (d + 1) (a:ls)

depth :: Stack a -> Int
depth (Stack d _) = d

snull :: Stack a -> Bool
snull (Stack _ ls) = null ls

spop :: Stack a -> Maybe (a, Stack a)
spop (Stack _ [])     = Nothing
spop (Stack d (a:ls)) = Just (a, Stack (d-1) ls)

instance Container Stack where
  empty  = sempty
  insert = sinsert
  nullc  = snull
  pop    = spop
