module Example2 where

import Graph


data Node2 =
    N1
  | N2
  | N3
  | N4
  | N5
  | N6
  deriving (Bounded, Eq, Ix, Ord, Show)


exampleGraph2 :: LabeledGraph Node2 Integer
exampleGraph2 =
    labeledGraph
      [ (N1 :-> N2,  7), (N1 :-> N3,  9), (N1 :-> N6, 14)
      , (N2 :-> N3, 10), (N2 :-> N4, 15)
      , (N3 :-> N4, 11), (N3 :-> N6,  2)
      , (N4 :-> N5,  6)
      , (N5 :-> N6,  9)
      ]
