module KleeneAlgebra
  ( module StarSemiring
  , KleeneAlgebra
  ) where

import StarSemiring


{- Laws:
   a <+> a = a
   a <.> x <+> x = x  ==>  star a <.> x <+> x = x
   x <.> a <+> x = x  ==>  x <.> star a <+> x = x
-}

class (StarSemiring a) => KleeneAlgebra a
