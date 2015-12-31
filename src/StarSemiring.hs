module StarSemiring
  ( module Semiring
  , StarSemiring
  , star
  , plus
  ) where

import Semiring


{- Laws:
   star a = one <+> a <.> star a = one <+> star a <.> a
-}

class (Semiring a) => StarSemiring a
  where
    star :: a -> a
    star a = one <+> plus a
    
    plus :: a -> a
    plus a = a <.> star a
