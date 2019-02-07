module Main where

import System.Environment
import Parser
import Control.Monad
import Evaluator
import Types
import Repl

-- import Control.Monad.Except

-- main :: IO () -- IO of () type
-- main = getArgs >>= print . extractValue . eval . readExpr . head
--main = do
--  args <- getArgs -- getArgs returns IO() === type of main, so we use '<-'
--  let a = readExpr (head args) >>= eval -- using bind to extract value from readExpr from withing ThrowsError and
--                                        -- passing to eval (Check the types) (What else could be done? use do inside do?
--                                        -- eval returns ThrowsError LispVal, not IO (), so used 'let'
--  -- let a = eval parsed -- a :: ThrowsError LispVal
--  let b = liftM show a -- b :: ThrowsError String
--  putStrLn $ extractValue $ trapError b

--main = do
--  args <- getArgs
--  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
--  putStrLn $ extractRightValue $ trapError evaled

main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> evalAndPrint $ args !! 0
               otherwise -> putStrLn "Program takes only 0 or 1 argument"