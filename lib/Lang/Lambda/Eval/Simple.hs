module  Lang.Lambda.Eval.Simple (convert,freeVars,allVars) where

import Control.Monad
import Control.Monad.Trans.State
import Data.Map.Strict (Map)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import Lang.Lambda.Types

{-| 
>>> fst (convert (App (Lam "x" (Var "x")) (Var "a")))
App (Lam 1 (Var 1)) (Var 2)

>>> fst (convert (App (App (Var "x") (Var "y")) (App (Var "a") (Var "b"))))
App (App (Var 1) (Var 2)) (App (Var 3) (Var 4))
-}
convert :: (Ord v,Traversable t) => t v -> (t Int,(Map v Int,Int))
convert = flip runState (M.empty,1) . traverse go 
    where 
    go v = do
        (vars,next) <- get
        case M.lookup v vars of
            Nothing -> do put (M.insert v next vars,succ next)
                          return next
            Just i -> return i
        
freeVars :: Term Int -> IntSet
freeVars (Var v) = IS.singleton v
freeVars (Lam v e) = IS.delete v (freeVars e)
freeVars (App f a) = freeVars f `IS.union` freeVars a

allVars :: Term Int -> IntSet
allVars (Var v) = IS.singleton v
allVars (Lam _ e) = allVars e
allVars (App f a) = allVars f `IS.union` allVars a
