module Evaluator where

import           Parser

-- @ matches with constructor components as well as the whole type
-- e.g. xs@(h:t) gives 3 bindings, complete list xs, head h and tail t
eval :: LispVal -> LispVal
eval val@(String _)             = val -- pattern matching input Lispval to String,
                          -- ignoring String constructor contents
eval val@(Number _)             = val
eval val@(Bool _)               = val
eval (List [Atom "quote", val]) = val -- refer parseQuoted
                                      -- matching against specific 2 member list,
                                      -- where 1st elem is the symbol quote
eval (List (Atom func:args))    = apply func $ map eval args -- eval a func, given a list of args

-- lookup looks the func among the primitives, if not found, returns false, else applies the args to it
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives -- what's the intuition for ($ args) ??

-- a list of pairs, function string and the operation it maps to
primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinOp (+))
  , ("-", numericBinOp (-))
  , ("*", numericBinOp (*))
  , ("/", numericBinOp div)
  , ("mod", numericBinOp mod)
  , ("quotient", numericBinOp quot)
  , ("remainder", numericBinOp rem)
  ]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op params = Number $ foldl1 op $ map unpackNum params

-- implement weak typing, ao a number, is taken as is even if it is written as string (quoted)
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)] -- normally reads returns a singleton list containing a tuple of int
                                              -- and remaining string, e.g. reads 34abc = [(34, abc)]
                                              -- in ambiguous cases it returns more than one pair
   in if null parsed -- null is a function that takes a list and returns if it's empty
        then 0
        else fst $ head parsed -- fun fact: else is madatory in haskell
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0
