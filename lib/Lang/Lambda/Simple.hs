module Lang.Lambda.Simple (LC(..)) where

import Text.ParserCombinators.ReadP

data LC v = Var v
          | Lam v (LC v)
          | App (LC v) (LC v)
          deriving (Eq,Show)

instance (Read v) => Read (LC v) where
    readsPrec _ = readP_to_S pLC

pLC, pLCAtom, pLCVar, pLCLam, pLCApp :: (Read v) => ReadP (LC v)
pLC = pLCLam +++ pLCApp +++ pLCLet

pLCVar = 
  do v <- pVar
     return $ Var v

pLCLam = 
  do schar '\\'
     v <- pVar
     schar '.'
     e <- pLC
     return $ Lam v e

pLCApp = 
  do es <- many1 pLCAtom
     return $ foldl1 App es

pLCAtom = 
    pLCVar 
    +++ 
    (do schar '('
        e <- pLC
        schar ')'
        return e)

pLCLet :: (Read v) => ReadP (LC v)
pLCLet =
  do sstring "let"
     bs <- sepBy pDef (schar ';')
     sstring "in"
     e <- pLC
     return $ foldr lcLet e bs
  where
    lcLet (x,e) b = App (Lam x b) e
    pDef = 
      do v <- pVar
         schar '='
         e <- pLC
         return (v,e)

schar :: Char -> ReadP Char
schar c = 
  do skipSpaces
     char c

sstring :: String -> ReadP String
sstring c =
  do skipSpaces
     string c

pVar :: (Read v) => ReadP v
pVar = 
  do skipSpaces
     readS_to_P (readsPrec 9)
