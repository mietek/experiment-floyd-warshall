module StarSemiringExp
  ( StarSemiringExp(..)
  ) where


data StarSemiringExp a =
    None
  | Empty
  | Var a
  | Star (StarSemiringExp a)
  | Or (StarSemiringExp a) (StarSemiringExp a)
  | Seq (StarSemiringExp a) (StarSemiringExp a)


instance (Show a) => Show (StarSemiringExp a)
  where
    showsPrec d None        = showParen (d > 10) (showString "0")
    showsPrec d Empty       = showParen (d > 10) (showString "ε")
    showsPrec d (Var a)     = showParen (d > 10) (shows a)
    showsPrec d (Star x)    = showParen  (d > 9) ( showsPrec 9 x
                                                 . showString "*"
                                                 )
    showsPrec d (x `Or` y)  = showParen  (d > 6) ( showsPrec 6 x
                                                 . showString "|"
                                                 . showsPrec 6 y
                                                 )
    showsPrec d (x `Seq` y) = showParen  (d > 7) ( showsPrec 7 x
                                                 . showsPrec 7 y
                                                 )
