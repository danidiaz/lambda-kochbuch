module  Lang.Lambda.Eval.Simple (convert,freeVars,allVars,subst,whnf,nf) where

import Control.Monad
import Control.Monad.Trans.State
import Data.Map.Strict (Map)
import Data.IntSet (IntSet)
import qualified Data.Map.Strict as M
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

{-| 
>>> nf (App (Lam 1 (Var 1)) (Var 2))
Var 2

>>> nf (App (Lam 1 (Var 3)) (Var 2))
Var 3

>>> nf (Lam 5 (App (Lam 1 (Var 1)) (Var 2)))
Lam 5 (Var 2)
-}
nf :: Term Int -> Term Int
nf e@(Var _) = e
nf (Lam x e) = Lam x (nf e)
nf (App f a) =
    case whnf f of
        Lam x b -> nf (subst x a b)
        f' -> App (nf f') (nf a)

{-| 
>>> whnf (Lam 5 (App (Lam 1 (Var 1)) (Var 2)))
Lam 5 (App (Lam 1 (Var 1)) (Var 2))

>>> whnf (App (Lam 6 (Lam 5 (App (Lam 1 (Var 1)) (Var 2)))) (Var 7))
Lam 5 (App (Lam 1 (Var 1)) (Var 2))
-}
whnf :: Term Int -> Term Int
whnf e@(Var _) = e
whnf e@(Lam _ _) = e
whnf (App f a) =
    case whnf f of
        Lam x b -> whnf (subst x a b)
        f' -> App f' a

newId :: IntSet -> Int
newId vs = head $ filter (flip IS.notMember vs) [0..]

subst :: Int -> Term Int -> Term Int -> Term Int
subst x s b = sub b
  where 
    sub e@(Var v) | v == x = s 
                  | otherwise = e
    sub e@(Lam v e') | v == x = e
                     | v `IS.member` fvs = Lam v' (sub e'')
                     | otherwise = Lam v (sub e')
                        where v' = newId vs
                              e'' = subst v (Var v') e'
    sub (App f a) = App (sub f) (sub a)
    fvs = freeVars s
    vs = fvs `IS.union` allVars b

