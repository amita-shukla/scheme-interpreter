module Main where

import System.Environment
import Parser
import Evaluator

main :: IO()
main = getArgs >>= print . eval . readExpr . head
