module Main where

import Infer
import Syntax

identity :: Term
identity = TmAbs "x" TUnit (TmVar "x")

x = infer emptyContext identity

main :: IO ()
main = print x
