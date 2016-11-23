module Control.Search.Internal.Depth
(
  targetDepth
, Depth(..)
) where

import Data.Word

targetDepth :: Word32 -> Depth
targetDepth = Limited

data Depth = Limited Word32 | NoLimit
