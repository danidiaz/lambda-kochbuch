{-# LANGUAGE FlexibleInstances #-}
module Lang.Lambda.Printer () where

import Control.Applicative
import Data.Functor.Identity
import Control.Monad
import Text.PrettyPrint.Leijen

import Lang.Lambda.Types

{- $setup

>>> :set -XOverloadedStrings
    
-}
