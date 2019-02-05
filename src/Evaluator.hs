{-# LANGUAGE ExistentialQuantification #-} -- for weak typing
module Evaluator where

import Types
import Control.Monad.Except

-- @ matches with constructor components as well as the whole type
-- e.g. xs@(h:t) gives 3 bindings, complete list xs, head h and tail t
eval :: LispVal -> ThrowsError LispVal
eval val@(String _)             = return val -- pattern matching input Lispval to String,
                          -- ignoring String constructor contents
eval val@(Number _)             = return val
eval val@(Bool _)               = return val
eval (List [Atom "quote", val]) = return val -- refer parseQuoted
                                      -- matching against specific 2 member list,
                                      -- where 1st elem is the symbol quote
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    otherwise -> eval conseq
eval (List (Atom func:args))    = mapM eval args >>= apply func -- eval a func, given a list of args


-- lookup looks the func among the primitives, if not found, returns false, else applies the args to it
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) $ lookup func primitives -- what's the intuition for ($ args) ??

-- a list of pairs, function string and the operation it maps to
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinOp (+))
  , ("-", numericBinOp (-))
  , ("*", numericBinOp (*))
  , ("/", numericBinOp div)
  , ("mod", numericBinOp mod)
  , ("quotient", numericBinOp quot)
  , ("remainder", numericBinOp rem),
  ("=", numBoolBinOp (==)),
  ("<", numBoolBinOp (<)),
  (">", numBoolBinOp (>)),
  ("/=", numBoolBinOp (/=)),
  (">=", numBoolBinOp (>=)),
  ("<=", numBoolBinOp (<=)),
  ("&&", boolBoolBinOp (&&)),
  ("||", boolBoolBinOp (||)),
  ("string=?", strBoolBinOp (==)),
  ("string<?", strBoolBinOp (<)),
  ("string>?", strBoolBinOp (>)),
  ("string<=?", strBoolBinOp (<=)),
  ("string>=?", strBoolBinOp (>=)),
  ("car", car),
  ("cdr", cdr),
  ("cons", cons),
  ("eq?", eqv),
  ("eqv?", eqv),
  ("equal?", equal)
  ]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
-- numericBinOp op params = Number $ foldl1 op $ mapM unpackNum params
-- numericBinOp op params = mapM unpackNum params >>= return . Number . foldl1 op
numericBinOp _ [] = throwError $ NumArgs 2 []
numericBinOp _ [x] = throwError $ NumArgs 2 [x]
numericBinOp op params = Number . foldl1 op <$> mapM unpackNum params -- <$> is fmap


-- implement weak typing, ao a number, is taken as is even if it is written as string (quoted)
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)] -- normally reads returns a singleton list containing a tuple of int
                                              -- and remaining string, e.g. reads 34abc = [(34, abc)]
                                              -- in ambiguous cases it returns more than one pair
   in if null parsed -- null is a function that takes a list and returns if it's empty
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ head parsed -- fun fact: else is mandatory in haskell
unpackNum (List [n]) = unpackNum n
unpackNum someLispVal = throwError $ TypeMismatch "type mismatch" someLispVal

{-
numBoolBinOp :: (Integer -> Integer -> Bool) -> LispVal -> LispVal -> ThrowsError LispVal
boolBoolBinOp :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> ThrowsError LispVal
strBoolBinOp :: (String -> String -> Bool) -> LispVal -> LispVal -> ThrowsError LispVal
-}

-- the above functions vary only in types
boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpack op params =
  if length params /= 2 -- /= is not equal to instead of !=
    then throwError $ NumArgs 2 params
    else do
      param1 <- unpack (params !! 0)
      param2 <- unpack (params !! 1)
      return $ Bool $ param1 `op` param2

numBoolBinOp  = boolBinOp unpackNum
strBoolBinOp  = boolBinOp unpackStr
boolBoolBinOp = boolBinOp unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool


car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList


cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) &&
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

{--
WEAK TYPING AND HETEROGENEOUS LISTS
-}
-- We want to introduce weak typing, so we want two elements to be equal even when they are of different types, e.g. 2 == "2"
-- We can map over the list of unpacker functions for this, and check equality of the inputs.
-- but this is not possible coz a list contains homogeneous types in haskell
-- So you define a common type Unpacker, s.t. all unpacker functions can be wrapped around this type.

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)



-- given a list of 2 LispVals, an Unpacker , tell if they are equal
--unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
--unpackEquals arg1 arg2 unpacker =
--  let
--    unpacked1 = unpacker arg1
--    unpacked2 = unpacker arg2
--  in return (unpacked1 == unpacked2)

----- WHEN TO USE LET AND WHEN TO USE DO??

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
     `catchError` const (return False)


equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

