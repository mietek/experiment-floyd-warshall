module Edge
  ( Edge(..)
  ) where

import Data.Array (Ix)


data Edge i =
    i :-> i
  deriving (Bounded, Eq, Ix, Ord)


instance (Show a) => Show (Edge a)
  where
    showsPrec _ (i :-> j) = showParen True ( shows i
                                           . (\s -> "->" ++ s)
                                           . shows j
                                           )
