module Main where

import Control.Monad (forM, forM_)
import Parser
import Typer

tests :: [String]
tests =
  [ "λx: Unit. x",
    "λx: Nat. λy: Nat. x",
    "λx: Nat. Succ x",
    "λx: Nat -> Nat. λy: Nat. (x y)",
    "λx: Nat -> Nat. x 0",
    "λf: Nat -> Nat. λx: Nat. f (f x)",
    "(λx: Nat. Succ x) 1",
    "λx: Nat -> Nat. λy: Nat. x (x y)",
    "λx: Nat -> (Nat -> Nat). λy: Nat. λz: Nat. x y z"
  ]

main :: IO ()
main = do
  forM_ tests $ \test -> do
    putStrLn $ "> " ++ test
    putStr "= "
    case runP test of
      Left err -> print err
      Right ast -> do
        case typeCheck emptyContext ast of
          Left err -> print err
          Right typ -> print typ

    putStrLn $ replicate 40 '-'
