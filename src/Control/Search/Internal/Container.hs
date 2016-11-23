{-# LANGUAGE TypeFamilies #-}

module Control.Search.Internal.Container where

newtype ContainerElem a = E {
    unElem :: a
  }

instance Underlying ContainerElem where
  underly = unElem

class Container c where
  type Elem c :: * -> *
  type instance Elem c = ContainerElem
  empty   :: c a
  insert  :: Elem c a -> c a -> c a
  nullc   :: c a -> Bool
  pop     :: c a -> Maybe (Elem c a, c a)

class Underlying c where
  underly :: c a -> a
