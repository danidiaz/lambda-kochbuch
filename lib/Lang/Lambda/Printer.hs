{-# LANGUAGE FlexibleInstances #-}
module Lang.Lambda.Printer (prettyTerm) where

import Control.Applicative
import Data.Functor.Identity
import Control.Monad
import Text.PrettyPrint.Leijen

import Lang.Lambda.Types

{- $setup

>>> :set -XOverloadedStrings
    
-}

{-| 
>>> putDoc (prettyTerm (App (Lam "x" (Var "x")) (Var "a")))
\x.x a

>>> putDoc (prettyTerm (App (App (Var "x") (Var "y")) (App (Var "a") (Var "b"))))
x y (x y)
-}
prettyTerm :: (Pretty v) => Term v -> Doc
prettyTerm (Var v) = pretty v
prettyTerm (Lam v t) = backslash <> pretty v<> dot <> prettyTerm t
prettyTerm (App t @t'(App {})) = prettyTerm t <+> lparen <> prettyTerm t' <> rparen
prettyTerm (App t t') = prettyTerm t <+> prettyTerm t' 
