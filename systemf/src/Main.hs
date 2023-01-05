module Main where

import Infer
import Syntax

-- identity : Λa.λx:a. x
identity :: Term
identity = TAbs "a" (Abs "x" (TVar "a") (Var "x"))

-- identity [Int]
x :: Term
x = TApp identity TInt

main :: IO ()
main = print (infer emptyContext x)
