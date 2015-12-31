module RegExp
  ( module KleeneAlgebra
  , RegExp
  , regExp
  , eval
  ) where

import KleeneAlgebra
import StarSemiringExp


newtype RegExp a =
    RegExp (StarSemiringExp a)


instance Show a => Show (RegExp a)
  where
    showsPrec d (RegExp x) = showsPrec d x


instance Semiring (RegExp a)
  where
    zero = RegExp None
    
    RegExp None     <+> y               = y
    x               <+> RegExp None     = x
    RegExp Empty    <+> RegExp Empty    = RegExp Empty
    RegExp Empty    <+> RegExp (Star y) = RegExp (Star y)
    RegExp (Star x) <+> RegExp Empty    = RegExp (Star x)
    RegExp x        <+> RegExp y        = RegExp (x `Or` y)
    
    one = RegExp Empty
    
    RegExp Empty <.> y            = y
    x            <.> RegExp Empty = x
    RegExp None  <.> _            = RegExp None
    _            <.> RegExp None  = RegExp None
    RegExp x     <.> RegExp y     = RegExp (x `Seq` y)


instance StarSemiring (RegExp a)
  where
    star (RegExp None)     = RegExp Empty
    star (RegExp Empty)    = RegExp Empty
    star (RegExp (Star x)) = star (RegExp x)
    star (RegExp x)        = RegExp (Star x)


instance KleeneAlgebra (RegExp a)


regExp :: a -> RegExp a
regExp =
    RegExp . Var


eval :: (KleeneAlgebra a) => (l -> a) -> RegExp l -> a
eval _ (RegExp None)        = zero
eval _ (RegExp Empty)       = one
eval f (RegExp (Var a))     = f a
eval f (RegExp (Star x))    = star (eval f (RegExp x))
eval f (RegExp (x `Or` y))  = (eval f (RegExp x)) <+> (eval f (RegExp y))
eval f (RegExp (x `Seq` y)) = (eval f (RegExp x)) <.> (eval f (RegExp y))
