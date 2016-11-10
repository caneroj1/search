module Control.Search.Internal.Path where

data Path a b = Node a (Maybe b) (Maybe Integer)
              | Path a (Maybe b) (Maybe Integer) (Path a b)
              deriving (Show, Eq, Ord)

type Level a b = [Path a b]

(-+-) :: Path a b -> Path a b -> Path a b
(Node a b c)   -+- p  = Path a b c p
(Path a b c p) -+- p' = Path a b c (p -+- p')

state :: Path a b -> a
state (Node a _ _)   = a
state (Path a _ _ _) = a
