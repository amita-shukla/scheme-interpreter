module Parser where

import System.Environment
-- importing module gets depency errors
-- stack install parsec
-- for stack project on intellij, add parsec to build-depends in cabal file
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!$%&|*+ -/: <=? >@^_~#"

-- >> is bind, can also be used to desugar do notation.
-- action1 >> action 2 is same as:
-- do
--   action1
--    action2
-- >> is used when you want to throw the result of prvs action.
-- >>= is used when you want to use the previous result in the next action.
readExpr :: String -> String
readExpr input =  case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space


-- DEFINING DATA TYPES
-- Every thing is of type LispVal in Scheme
data LispVal = Atom String -- atom is variable name
             | List [LispVal]
             | DottedList [LispVal] LispVal -- a list of all elements but the last
             | Number Integer
             | String String -- constructor tags and types can have same name
             | Bool Bool


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
      "f" -> Bool False
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
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber
