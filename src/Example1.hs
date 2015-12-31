module Example1 where

import Graph


data Node1 =
    NA
  | NB
  | NC
  | ND
  | NE
  deriving (Bounded, Eq, Ix, Ord, Show)


exampleGraph1 :: Graph Node1
exampleGraph1 =
    graph
      [ (NA :-> NB)
      , (NB :-> NC)
      , (NC :-> ND)
      , (NC :-> NE)
      , (ND :-> NB)
      , (NE :-> ND)
      ]
