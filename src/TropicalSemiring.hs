module TropicalSemiring
  ( module KleeneAlgebra
  , TropicalSemiring(..)
  ) where

import KleeneAlgebra


data TropicalSemiring a =
    TropicalSemiring a -- where a is non-negative
  | Infinity
  deriving (Eq, Ord)


instance (Show a) => Show (TropicalSemiring a)
  where
    show (TropicalSemiring a) = show a
    show Infinity             = "∞"


instance (Ord a, Num a) => Semiring (TropicalSemiring a)
  where
    zero = Infinity
    
    Infinity             <+> y                    = y
    x                    <+> Infinity             = x
    (TropicalSemiring a) <+> (TropicalSemiring b) = TropicalSemiring (min a b)
    
    one = TropicalSemiring 0
    
    Infinity             <.> _                    = Infinity
    _                    <.> Infinity             = Infinity
    (TropicalSemiring x) <.> (TropicalSemiring y) = TropicalSemiring (x + y)


instance (Ord a, Num a) => StarSemiring (TropicalSemiring a)
  where
    star _ = one


instance (Ord a, Num a) => KleeneAlgebra (TropicalSemiring a)
