{-# LANGUAGE FlexibleInstances #-}
module Lang.Lambda.Parser (Name,parseTerm,parseTermDebug,DebugMessage(..)) where

import Control.Applicative
import Data.Text (Text)
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Text.Megaparsec

import Lang.Lambda

{- $setup

>>> :set -XOverloadedStrings
    
-}

type Name = String

type Parser m v = ParsecT Dec Text m v

data DebugMessage = Msg String 
                  | AName String
                  deriving (Eq,Show)

class Monad m => MonadDebug m where
    debug :: DebugMessage -> m () 

instance MonadDebug Identity where
    debug _ = pure () 

instance Monad m => MonadDebug (WriterT [DebugMessage] m) where
    debug msg = tell [msg]

instance (MonadDebug m,Stream t,ErrorComponent d) => MonadDebug (ParsecT d t m) where
    debug = lift . debug

-- Pending problem: 
-- Lang.Lambda.Simple> parseTermTest " let x = foo in x"
-- App (Var "let") (Var "x")
-- Make let and in reserved words
-- parseTerm "let x = a b; in x"
termParser :: MonadDebug m => Parser m (Term Name)
termParser = try lambdaParser
         <|> try appParser
         <|> try letParser 

lambdaParser :: MonadDebug m => Parser m (Term Name)
lambdaParser = 
  do schar '\\'
     v <- varParser
     schar '.'
     e <- termParser
     return $ Lam v e

appParser :: MonadDebug m => Parser m (Term Name)
appParser = do
    es <- liftA2 (:) atomParser (many atomParser)
    return $ foldl1 App es

atomParser :: MonadDebug m => Parser m (Term Name)
atomParser = 
    try (Var <$> varParser)
    <|> 
    try (do schar '('
            e <- termParser
            schar ')'
            return e)

letParser :: MonadDebug m => Parser m (Term Name)
letParser = do
    sstring "let"
    bs <- sepBy1 defParser 
                 (char ';')
    sstring "in"
    e <- termParser
    return $ foldr (\(x,e') b -> App (Lam x b) e') e bs
  where
    defParser = do
        v <- varParser
        debug $ Msg ("var " ++ show v)
        schar '='
        e <- termParser
        debug $ Msg ("term " ++ show e)
        space
        return (v,e)

reservedWords :: [String] 
reservedWords = ["let","in"]

varParser :: MonadDebug m => Parser m Name
varParser = do
    space
    identifier <- (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    debug $ AName identifier 
    if identifier `elem` reservedWords
    then do debug $ Msg ("oops " ++ show identifier)
            fail $ "keyword " ++ show identifier ++ " cannot be an identifier"
    else return identifier

schar :: Char -> Parser m Char
schar c = 
  do space
     char c

sstring :: String -> Parser m String
sstring c =
  do space
     string c

-- Lang.Lambda.Simple> parseTerm  "let x = a b ; y = z in x"
-- Lang.Lambda.Simple> parseTerm  "let x = a b in x"
parseTermDebug :: Text -> (Either (ParseError Char Dec) (Term Name),[DebugMessage])
parseTermDebug text = runWriter (runParserT termParser "" text)

{-| 

>>> parseTerm  "let x = a b in f x"
Right (App (Lam "x" (App (Var "f") (Var "x"))) (App (Var "a") (Var "b")))

>>> parseTerm  "let x = a ; y = b in x"
Right (App (Lam "x" (App (Lam "y" (Var "x")) (Var "b"))) (Var "a"))

>>> parseTerm  "\\a.\\b.a b"
Right (Lam "a" (Lam "b" (App (Var "a") (Var "b"))))

>>> parseTerm  "a (b c)"
Right (App (Var "a") (App (Var "b") (Var "c")))

-}
parseTerm :: Text -> Either (ParseError Char Dec) (Term Name)
parseTerm = fst . parseTermDebug

