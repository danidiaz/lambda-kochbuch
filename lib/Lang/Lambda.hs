module Lang.Lambda (Term(..)) where

data Term v = Var v
            | Lam v (Term v)
            | App (Term v) (Term v)
            deriving (Eq,Show)

