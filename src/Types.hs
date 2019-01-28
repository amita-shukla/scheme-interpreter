module Types where

import Text.ParserCombinators.Parsec
import Control.Monad.Except

-- DEFINING DATA TYPES
-- Every thing is of type LispVal in Scheme
data LispVal = Atom String -- atom is variable name
             | List [LispVal]
             | DottedList [LispVal] LispVal -- a list of all elements but the last
             | Number Integer
             | String String -- constructor tags and types can have same name
             | Bool Bool

-- I don't know what's a proper place to put this
-- has put this in Evaluator.hs, gave the error:
-- "No instance for (Show LispVal) arising from a use of show
showVal :: LispVal -> String
showVal (Atom name) = name
showVal (Number num) = show num
showVal (String s) = s
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordList head ++ " . " ++ showVal tail ++ ")"

-- glue together a list of words with spaces, written in pointfree style
unwordList :: [LispVal] -> String -- could not use Parser.List here, error: Parser doesnot export List
unwordList = unwords . map showVal

-- calling show on LispVal should display its contents
instance Show LispVal where show = showVal

-- create a data type for errors
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

-- create a type that is returned by all the functions which can throw errors.
-- so, if f :: _ -> Integer, now it should be f :: _ -> Either LispError Integer
type ThrowsError = Either LispError -- create a curried type Throws error, now f :: _ -> ThrowsError Integer

-- create a function that catches a value and if it's an error, then convert it into string
-- trapError action = catchError action (return . show)

showError :: LispError -> String
showError (NumArgs expected argsFound) = "Expected: " ++ show expected ++ " arguments, found values: " ++ show argsFound
showError (TypeMismatch expected found) = "Invalid type: " ++ show expected ++ ", found:" ++ show found
showError (Parser parseError) = "Parse error at: " ++ show parseError
showError (BadSpecialForm message found) = message ++ ": " ++ show found
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname)  = message ++ ": " ++ varname

instance Show LispError where show = showError

-- after adding error handling, we get responses as Left or Right
-- e.g. "(+ 2 3)" gives Right 5; "(+2)" gives Left Expected 2, found values [2]
-- so we convert the response of eval to String and print it

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


-- is response is an error, catch it and show it
-- catchError :: MonadError e m => m a -> (e -> m a) -> m a
-- trapError  :: (Control.Monad.Error.Class.MonadError a m, Show a) => m String -> m String
trapError response = catchError response (return . show)

-- is still don't know if I have got the intuition for trapError. Shouldn't there be a better way to do all this??
-- TODO: and error handling stuff needs to go to a different file.