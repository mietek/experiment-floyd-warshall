module Connection
  ( module KleeneAlgebra
  , Connection(..)
  ) where

import KleeneAlgebra


data Connection =
    Connected
  | Unconnected
  deriving (Eq)


instance Show Connection
  where
    show Connected   = "*"
    show Unconnected = "0"


instance Semiring Connection
  where
    zero = Unconnected
    
    Connected   <+> _ = Connected
    Unconnected <+> x = x
    
    one = Connected
    
    Unconnected <.> _ = Unconnected
    Connected   <.> x = x


instance StarSemiring Connection
  where
    star _ = one


instance KleeneAlgebra Connection
