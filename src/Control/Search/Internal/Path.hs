module Control.Search.Internal.Path where

type Cost = Double

data Path a b = Node a (Maybe b) Cost
              | Path a (Maybe b) Cost (Path a b)
              deriving (Show, Eq)

data WeightedPath a b = WP Cost (Path a b)

type Level a b = [Path a b]
type WeightedLevel a b = [WeightedPath a b]

unwrap :: WeightedPath a b -> Path a b
unwrap (WP _ p) = p

instance (Eq a) => Eq (WeightedPath a b) where
  (WP _ (Node s1 _ _))   == (WP _ (Node s2 _ _))   = s1 == s2
  (WP _ (Node s1 _ _))   == (WP _ (Path s2 _ _ _)) = s1 == s2
  (WP _ (Path s1 _ _ _)) == (WP _ (Node s2 _ _))   = s1 == s2
  (WP _ (Path s1 _ _ _)) == (WP _ (Path s2 _ _ _)) = s1 == s2

instance (Ord a) => Ord (WeightedPath a b) where
  (WP _ (Node s1 _ _))   `compare` (WP _ (Node s2 _ _))   = compare s1 s2
  (WP _ (Node s1 _ _))   `compare` (WP _ (Path s2 _ _ _)) = compare s1 s2
  (WP _ (Path s1 _ _ _)) `compare` (WP _ (Node s2 _ _))   = compare s1 s2
  (WP _ (Path s1 _ _ _)) `compare` (WP _ (Path s2 _ _ _)) = compare s1 s2


(-+-) :: Path a b -> Path a b -> Path a b
(Node a b c)   -+- p  = Path a b c p
(Path a b c p) -+- p' = Path a b c (p -+- p')

state :: Path a b -> a
state (Node a _ _)   = a
state (Path a _ _ _) = a

pathCost :: WeightedPath a b -> Double
pathCost (WP c _) = c
