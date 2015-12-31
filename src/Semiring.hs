module Semiring
  ( Semiring
  , (<+>)
  , (<.>)
  , zero
  , one
  , semiringSum
  , semiringProduct
  ) where


infixl 6 <+>
infixl 7 <.>


{- Laws:
   a <+> b         = b <+> a
   (a <+> b) <+> c = a <+> (b <+> c)
   a <+> zero      = zero <+> a          = a
   (a <.> b) <.> c = a <.> (b <.> c)
   a <.> one       = one <.> a           = a
   a <.> zero      = zero <.> a          = zero
   a <.> (b <+> c) = a <.> b <+> a <.> c
   (a <+> b) <.> c = a <.> c <+> b <.> c
-}

class Semiring a
  where
    zero :: a
    
    (<+>) :: a -> a -> a
    
    one :: a
    
    (<.>) :: a -> a -> a
    
    semiringSum :: [a] -> a
    semiringSum = foldr (<+>) zero
    
    semiringProduct :: [a] -> a
    semiringProduct = foldr (<.>) one
