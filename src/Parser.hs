module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except
import Types

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- >> is bind, can also be used to desugar do notation.
-- action1 >> action 2 is same as:
-- do
--   action1
--    action2
-- >> is used when you want to throw the result of prvs action.
-- >>= is used when you want to use the previous result in the next action.
readExpr :: String -> ThrowsError LispVal
readExpr input =  case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

-- We’re back to using the do-notation instead of the >> operator.
-- This is because we’ll be retrieving the value of our parse (returned by many (noneOf
-- "¨")) and manipulating it, interleaving some other parse operations in the meantime.
-- In general, use >> if the actions don’t return a value
-- , >>= if you’ll be immediately passing that value into the next action,
-- and do-notation otherwise.
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x -- return :: a -> ma, return here wraps LispVal (String) val into Parser.
                                   -- Each constructor in a ADT acts like a function that turns value into type

-- an atom is a letter or symbols, followed by any number of letter or symbols
-- <|> is a choice in Parsec
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _ -> Atom atom

-- many1 returns 1 or more digits returns a Parser String
-- but we need an number, so we need to call read on the string
-- and wrap it in a Number.
-- read a :: String -> a
-- Number :: Int -> LispVal
-- So we compose Number . read :: String -> LispVal
-- liftM operates on value wrapped inside Parser
-- liftM :: Monad m => (a->b) -> m a -> m b
parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
-- ex1:
parseNumber = do
  a <- many1 digit
  return $ (Number . read) a

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> do
        char '('
        x <- try parseList <|> parseDottedList
        char ')'
        return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces -- endby parse zero or more occurences of parsers, separated and ended by spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

-- support for single quote syntactic sugar
-- adding a single quote in front of an atom
-- enables scheme to treat the atom as literal, intead of
-- looking for its binding to a variable name
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x] -- 'quote' x, returns x as LispVal



