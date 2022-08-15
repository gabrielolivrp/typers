module Main where

import Infer
import Syntax

-- id' : ∀a. a -> a
id' = TmAbs "x" (TmVar "x")

-- id' 1 : Int
appId = TmApp id' (TmLit (LInt 1))

-- foo : ∀a b. a -> b -> a
foo = TmAbs "x" (TmAbs "y" (TmVar "x"))

-- foo False : ∀b. b -> Bool
appFoo = TmApp foo (TmLit (LBool False))

x = runInfer $ infer emptyContext appId

main :: IO ()
main = putStrLn "Hello, Haskell!"
