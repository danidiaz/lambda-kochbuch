{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Lang.Lambda.Types (Term(..)) where

data Term v = Var v
            | Lam v (Term v)
            | App (Term v) (Term v)
            deriving (Eq,Show,Functor,Foldable,Traversable)

