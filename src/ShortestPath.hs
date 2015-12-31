module ShortestPath
  ( module KleeneAlgebra
  , ShortestPath
  , extract
  , annotate
  , labels
  , distances
  , distances'
  , shortestPathsRegExps
  , shortestPaths
  ) where

import Graph
import KleeneAlgebra
import Language
import RegExp
import TropicalSemiring


data ShortestPath a b =
    ShortestPath (TropicalSemiring a) b


instance (Show a, Show b) => Show (ShortestPath a b)
  where
    show (ShortestPath a x) =
        show x ++ "[" ++ show a ++ "]"


instance Functor (ShortestPath a)
  where
    fmap f (ShortestPath a x) =
        ShortestPath a (f x)


instance (Ord a, Num a, Semiring b) => Semiring (ShortestPath a b)
  where
    zero =
        ShortestPath zero zero
    
    ShortestPath a x <+> ShortestPath b y
      | c < b     = ShortestPath a x
      | c < a     = ShortestPath b y
      | otherwise = ShortestPath c (x <+> y)
      where
        c = a <+> b
                        
    one =
        ShortestPath one one
  
    ShortestPath a x <.> ShortestPath b y =
        ShortestPath (a <.> b) (x <.> y)


instance (Ord a, Num a, StarSemiring b) => StarSemiring (ShortestPath a b)
  where
    star (ShortestPath x b)
      | x == one  = ShortestPath one (star b)
      | otherwise = ShortestPath one one


instance (Ord a, Num a, KleeneAlgebra b) => KleeneAlgebra (ShortestPath a b)


extract :: ShortestPath a b -> b
extract (ShortestPath _ x) = x


annotate :: (Ix i, Bounded i, Ord a, Num a, Semiring b)
         => ((Edge i) -> b)
         -> LabeledGraph i a
         -> Matrix i (ShortestPath a b)
annotate f m =
    go <$> m <*> labelWithEdge (unlabel m)
  where
    go v e =
        ShortestPath (maybe zero TropicalSemiring v) (maybe zero f e)


labels :: (Functor f, Num a, Ord a)
       => f (Maybe a)
       -> f (TropicalSemiring a)
labels =
    fmap (maybe zero TropicalSemiring)


distances :: (Functor f, Num a, Ord a, StarSemiring (f (TropicalSemiring a)))
          => f (Maybe a)
          -> f (TropicalSemiring a)
distances =
    star . fmap (maybe zero TropicalSemiring)


distances' :: (Num a, Ord a, Bounded i, Ix i)
           => LabeledGraph i a
           -> Matrix i (TropicalSemiring a)
distances' =
    fmap (eval TropicalSemiring) . star . regExpMap


shortestPathsRegExps :: (Num a, Ord a, Bounded i, Ix i)
                     => LabeledGraph i a
                     -> Matrix i (ShortestPath a (RegExp (Edge i)))
shortestPathsRegExps =
    star . annotate regExp


shortestPaths :: (Num a, Ord a, Bounded i, Ix i)
              => LabeledGraph i a
              -> LabeledGraph i [Edge i]
shortestPaths =
    fmap (someWord . extract) . star . annotate letter
