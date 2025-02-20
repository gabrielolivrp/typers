{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad (forM_)
import Parser
import Typer

tests :: [String]
tests =
  [ "Λa. λx: a. x",
    "(Λa. λx: a. x) [Nat]",
    "(Λa. Λb. λx: a. λy: b. x) [Nat] [Unit]",
    "(λf: Nat -> Nat. λx: Nat. f x) ((Λa. λy: a. y) [Nat])",
    "Λa. λf: a -> a. λx:  a. f (f x)",
    "(Λa. λx: a. x) [Nat -> Nat]",
    "(Λa. λx: a. x) [(Nat -> Nat) -> Nat]",
    "(Λa. Λb. λx: b -> a. λy: b. x y) [Nat -> Unit] [Nat]",
    "(λx: Nat -> Nat. x) (Λb. λy: b. y) [Nat]",
    "(Λa. λx: a. x) [Nat -> Nat]"
  ]

main :: IO ()
main = do
  forM_ tests $ \test -> do
    putStrLn $ "> " ++ test
    putStr "= "
    case runP test of
      Left err -> print err
      Right term ->
        case infer emptyContext term of
          Left err -> print err
          Right typ -> print typ

    putStrLn $ replicate 40 '-'
