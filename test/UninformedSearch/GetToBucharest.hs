module UninformedSearch.GetToBucharest where

import Control.Search.Internal.Path
import Control.Search.Types
import Control.Search.BFS
import Data.Set (Set)
import qualified Data.Set as Set hiding (Set)

type Answer = Path Location Drive

data Location = Sibiu
              | Fagaras
              | RimnicuVilcea
              | Pitesti
              | Bucharest
              deriving (Ord, Eq, Show)

data Drive = Drive deriving (Ord, Eq, Show)

type GetToBucharest = Searchable Location Drive
