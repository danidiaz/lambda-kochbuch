module Lang.Lambda.Simple (Term(..)) where

import Control.Applicative
-- import Data.Text(Text)
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as Lexer

data Term v = Var v
            | Lam v (Term v)
            | App (Term v) (Term v)
            deriving (Eq,Show)

type Name = String

termParser :: Parser (Term Name)
termParser = try lambdaParser
         <|> try appParser
         <|> try letParser

varParser :: Parser Name
varParser = (:) <$> letterChar <*> many (alphaNumChar <|> char '_') 

schar :: Char -> Parser Char
schar c = 
  do space
     char c

lambdaParser :: Parser (Term Name)
lambdaParser = 
  do schar '\\'
     v <- varParser
     schar '.'
     e <- termParser
     return $ Lam v e

appParser :: Parser (Term Name)
appParser = do
    es <- liftA2 (:) atomParser (many atomParser)
    return $ foldl1 App es
    

atomParser :: Parser (Term Name)
atomParser = 
    try (do space
            Var <$> varParser)
    <|> 
    try (do schar '('
            e <- termParser
            schar ')'
            return e)

letParser :: Parser (Term Name)
letParser = undefined

-- instance (Read v) => Read (Term v) where
--     readsPrec _ = readP_to_S pTerm
-- 
-- pTerm, pTermAtom, pTermVar, pTermLam, pTermApp :: (Read v) => ReadP (Term v)
-- pTerm = pTermLam +++ pTermApp +++ pTermLet
-- 
-- pTermVar = 
--   do v <- pVar
--      return $ Var v
-- 
-- pTermLam = 
--   do schar '\\'
--      v <- pVar
--      schar '.'
--      e <- pTerm
--      return $ Lam v e
-- 
-- pTermApp = 
--   do es <- many1 pTermAtom
--      return $ foldl1 App es
-- 
-- pTermAtom = 
--     pTermVar 
--     +++ 
--     (do schar '('
--         e <- pTerm
--         schar ')'
--         return e)
-- 
-- pTermLet :: (Read v) => ReadP (Term v)
-- pTermLet =
--   do sstring "let"
--      bs <- sepBy pDef (schar ';')
--      sstring "in"
--      e <- pTerm
--      return $ foldr lcLet e bs
--   where
--     lcLet (x,e) b = App (Lam x b) e
--     pDef = 
--       do v <- pVar
--          schar '='
--          e <- pTerm
--          return (v,e)
-- 
-- schar :: Char -> ReadP Char
-- schar c = 
--   do skipSpaces
--      char c
-- 
-- sstring :: String -> ReadP String
-- sstring c =
--   do skipSpaces
--      string c
-- 
-- pVar :: (Read v) => ReadP v
-- pVar = 
--   do skipSpaces
--      readS_to_P (readsPrec 9)
