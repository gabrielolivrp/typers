module Main where

import Infer
import Syntax

id' = TmAbs "x" (TmVar "x")

idLet = TmLet "x" (TmApp id' (TmLit (LInt 1))) (TmVar "x")

x = runInfer $ infer emptyContext idLet

main :: IO ()
main = putStrLn "Hello, Haskell!"
