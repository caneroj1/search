module Control.Search.BFS where

import Control.Search.Types
import Control.Search.Internal.Queue
--
-- bfs :: (Searchable a) => a -> Maybe [(State a, Maybe (Action a), Maybe Integer)]
-- bfs p
--   | goal is p = Just [(is, Nothing, Just 0)]
--   | otherwise = _bfs p initialFrontier emptySet Nothing
--   where
--     is              = initialState p
--     initialFrontier = is `insert` Seq.empty
--
-- _bfs :: (Searchable a)
--      => a
--      -> Queue (State a)
--      -> ExploredSet a
--      -> Maybe (Action a)
--      -> Maybe [(State a, Maybe (Action a), Maybe Integer)]
-- _bfs p frontier exploredSet actionTaken
--   | empty frontier = Nothing
--   | otherwise      = Nothing--pure (:) <$> Just (f, actionTaken) <*>
--   where
--     Just (f, frontier') = pop frontier
--     exploredSet'        = explore f exploredSet
--     acts                = actions f p
--     nxts                =
--       concatMap (\a -> zip (follows f a p) (repeat a) ) acts -- filter here
--     tryEach ((nextSt, a):ps)
--       | goal nextSt p = Just [(nextSt, Just a, c)]
--       | otherwise     = pure (:) <$> Just (f, actionTaken, c)
--                                  <*> _bfs p frontier exploredSet' (Just a)
--       where
--         c = cost f a nextSt p
