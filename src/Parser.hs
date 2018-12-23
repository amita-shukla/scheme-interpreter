module Parser where

import System.Environment
-- importing module gets depency errors
-- stack install parsec
-- for stack project on intellij, add parsec to build-depends in cabal file
import Text.ParserCombinators.Parsec hiding (spaces)

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
readExpr input =  case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space
