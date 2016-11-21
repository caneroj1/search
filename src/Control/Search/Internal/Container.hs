module Control.Search.Internal.Container where

class Container c where
  empty   :: c a
  insert  :: a -> c a -> c a
  nullc   :: c a -> Bool
  pop     :: c a -> Maybe (a, c a)
