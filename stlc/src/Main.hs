module Main where

import qualified Env as E
import Infer
import Types

identity :: Term
identity = TmAbs "x" TyBool (TmVar "x")

x = infer E.empty identity

main :: IO ()
main = print x
