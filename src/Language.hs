module Language
  ( module KleeneAlgebra
  , Language
  , letter
  , someWord
  ) where

import Data.Maybe (listToMaybe)

import KleeneAlgebra


newtype Language a =
    Language [[a]]
  deriving (Show)


instance Semiring (Language a)
  where
    zero = Language []
    
    (Language x) <+> (Language y) = Language (interleave x y)
        
    one = Language (pure [])
    
    (Language x) <.> (Language y) = Language (dovetail (++) x y)
        

instance StarSemiring (Language a)
  where
    star (Language l) = one <+> plusList (filter (not . null) l)


instance KleeneAlgebra (Language a)


letter :: a -> Language a
letter x = Language [[x]]


someWord :: Language a -> Maybe [a]
someWord (Language l) = listToMaybe l


interleave :: [a] -> [a] -> [a]
interleave []       ys = ys
interleave (x : xs) ys = x : (interleave ys xs)


dovetail :: (a -> b -> a1) -> [a] -> [b] -> [a1]
dovetail f l1 l2 =
    concat (go l1 (scanl (flip (:)) [] l2))
  where
    go []         _            = []
    go _          []           = []
    go l          (x : y : ys) = (zipWith f l x) : (go l (y : ys))
    go l@(_ : as) [x]          = (zipWith f l x) : (go as [x])


plusList :: [[a]] -> Language a
plusList [] = zero
plusList l  = star (Language l) <.> (Language l)
