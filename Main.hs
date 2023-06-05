module Main where

import DPLL (satisfiable)
import Expr (convertToCnf)
import Test (tests)

main :: IO ()
main = print $ map (satisfiable . convertToCnf) tests
