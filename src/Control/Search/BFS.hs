module Control.Search.BFS
(
  bfs
) where

import Control.Search.Internal.Container
import Control.Search.Internal.DepthSearch (isAtGoal)
import Control.Search.Internal.ExploredSet
import qualified Control.Search.Internal.ExploredSet as E (empty)
import Control.Search.Internal.Path
import Control.Search.Internal.Frontier
import Control.Search.Internal.Queue
import Control.Search.Types
import Data.List
import Data.Maybe

bfs :: (Ord state)
    => Searchable state action
    -> Maybe (Path state action)
bfs p = search p (mkFrontier `addFrontier` is p) E.empty
  where
    is       p = E $ Node (initialState p) Nothing 0

search :: (Ord state)
        => Searchable state action
        -> Frontier Queue state action
        -> ExploredSet state
        -> Maybe (Path state action)
search p frontier explored
  | isNothing mbf          = Nothing
  | isExplored st explored = search p frontier' explored
  | otherwise              = either Just doNextSearch children
  where
    mbf                  = headFrontier frontier
    Just (ce, frontier') = mbf
    path                 = underly ce
    st                   = state path
    children             = makeChildren p path
    mkFrontier           = foldl' addFrontier frontier'
    doNextSearch cs      = search p (mkFrontier cs) (explore st explored)

makeChildren :: Searchable state action
             -> Path state action
             -> Either (Path state action)
                       [ContainerElem (Path state action)]
makeChildren p path = sequence $ do
  a <- actions p st
  s <- follows p st a
  let nc    = costFunction p st a s
      path' = Node s (Just a) nc -+- path
  if isAtGoal p path' then
    [Left path']      else
    [Right $ E path']
  where
    st = state path
