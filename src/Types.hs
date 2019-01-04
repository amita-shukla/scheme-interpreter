module Types where


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
showVal (Parser.String s) = s
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordList head ++ " . " ++ showVal tail ++ ")"

-- glue together a list of words with spaces, written in pointfree style
unwordList :: [LispVal] -> String -- could not use Parser.List here, error: Parser doesnot export List
unwordList = unwords . map showVal

-- calling show on LispVal should display its contents
instance Show LispVal where show = showVal
