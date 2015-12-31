module Matrix
  ( Ix
  , module Edge
  , module KleeneAlgebra
  , Matrix
  , matrix
  , transpose
  ) where

import Control.Applicative (liftA2)
import Data.Array (Array, Ix, (!), listArray, range)

import Edge
import KleeneAlgebra


newtype Matrix i a =
    Matrix (Array (Edge i) a)
  deriving (Eq)


instance (Ix i, Bounded i, Show a) => Show (Matrix i a)
 where
   show (Matrix m) =
       unlines [ concat [ pad (m' ! (i :-> j)) j
                        | j <- entireRange
                        ]
               | i <- entireRange
               ]
     where
       pad s j = s ++ replicate ((len j) - (length s) + 1) ' '
       len j   = maximum [ lenm ! (i :-> j)
                         | i <- entireRange
                         ]
       lenm    = fmap length m'
       m'      = fmap show m


instance (Ix i) => Functor (Matrix i)
  where
    fmap f (Matrix m) = Matrix (fmap f m)


instance (Ix i, Bounded i) => Applicative (Matrix i)
  where
    pure x = matrix (const x)
    
    Matrix f <*> Matrix x = matrix $ \(i :-> j) ->
                              (f ! (i :-> j)) (x ! (i :-> j))


instance (Ix i, Bounded i, Semiring a) => Semiring (Matrix i a)
  where
    zero = pure zero
    
    (<+>) = liftA2 (<+>)
    
    one = matrix $ \(i :-> j) -> if i == j then one else zero
    
    Matrix x <.> Matrix y = matrix build
      where
        build (i :-> j) =
            semiringSum [ x ! (i :-> k) <.> y ! (k :-> j)
                        | k <- entireRange
                        ]


instance (Ix i, Bounded i, StarSemiring a) => StarSemiring (Matrix i a)
  where
    plus x = foldr f x entireRange
      where
        f k (Matrix m) = matrix build
          where
            build (i :-> j) =
                m ! (i :-> j) <+>
                m ! (i :-> k) <.> star (m ! (k :-> k)) <.> m ! (k :-> j)


instance (Ix i, Bounded i, KleeneAlgebra a) => KleeneAlgebra (Matrix i a)


matrix :: (Ix i, Bounded i) => (Edge i -> a) -> Matrix i a
matrix f =
    Matrix (listArray (minBound, maxBound) (map f entireRange))


transpose :: (Ix i, Bounded i) => Matrix i a -> Matrix i a
transpose (Matrix m) =
    matrix $ \(i :-> j) ->
      m ! (j :-> i)


entireRange :: (Ix i, Bounded i) => [i]
entireRange =
    range (minBound, maxBound)
