module Repl where

import System.IO
import Parser
import Evaluator
import Types
import Control.Monad

runRepl :: IO()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

-- read input
-- perform a function
-- print the output
-- all in an infinite loop
-- io string : until_ :: (string -> bool) -> io string -> (string -> io ()) -> io ()
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
    then return () -- if user has entered "quit"
    else action result >> until_ predicate prompt action -- eval and print result, and then
                                                        -- call until_ again


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout -- hFlush immediately flushes stdout

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError $ liftM show $ readExpr expr >>= eval

