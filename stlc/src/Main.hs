module Main where

import qualified Env as E
import Infer
import Types

identity :: Term
identity = TmAbs (Ident "x") TyBool (TmVar (Ident "x"))

x = infer E.empty identity

main :: IO ()
main = print x
