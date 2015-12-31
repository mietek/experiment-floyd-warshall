module Example3 where

import Graph


data Node3 =
    N Int
  deriving (Eq, Ix, Ord)


instance Bounded Node3
  where
    minBound = N 0
    maxBound = N 7


instance Show Node3
  where
    show (N k) = "N" ++ show k


exampleGraph3 :: LabeledGraph Node3 Double
exampleGraph3 =
    labeledGraph $
        [ (N 1 :-> N 2,  7), (N 1 :-> N 3,  9), (N 1 :-> N 6, 14)
        , (N 2 :-> N 3, 10), (N 2 :-> N 4, 15)
        , (N 3 :-> N 4, 11), (N 3 :-> N 6,  2)
        , (N 4 :-> N 5,  6)
        , (N 5 :-> N 6,  9)
        ]


exampleGraph3' :: LabeledGraph Node3 Double
exampleGraph3' =
    labeledGraph'
      [ (N 1, [(N 2,  7), (N 3,  9), (N 6, 14)])
      , (N 2, [(N 3, 10), (N 4, 15)])
      , (N 3, [(N 4, 11), (N 6,  2)])
      , (N 4, [(N 5,  6)])
      , (N 5, [(N 6,  9)])
      ]
