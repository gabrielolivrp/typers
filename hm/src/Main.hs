module Main where

import Control.Monad (forM_)
import Parser (runP)
import Typer

tests :: [String]
tests =
  [ "λx. x",
    "λf. λx. f x",
    "λf. λx. λy. f x y",
    "let id = λx. x in id False",
    "let id = λx. x in id 0",
    "let id = λx. x in id (id 1)",
    "let f = λx. x in let g = λy. y in f (g True)",
    "let comp = λf. λg. λx. f (g x) in comp",
    "let const = λx. λy. x in const 42 True",
    "let f = λx. x in f 123",
    "let b = True in b",
    "let id = λx. x in let apply = λf. λx. f x in apply id 42",
    "let twice = λf. λx. f (f x) in twice (λx. x) 0",
    "let flip = λf. λx. λy. f y x in flip",
    "let curry = λf. λx. λy. f x y in curry (λx. x)",
    "let g = λx. x in let f = λy. g y in f 42",
    "let id = λx. x in let k = λx. λy. x in id (k 1 2)",
    "let s = λx. λy. λz. x z (y z) in s",
    "let poly = λx. x in poly True",
    "let poly = λx. x in let f = poly 42 in poly True",
    "let f = λx. (λy. x) in f 42",
    "(λx. x) ((λy. y) 42)",
    "let f = λx. λy. x in f 1 2",
    "(λx. λy. x y) (λz. z) 42",
    "let t = True in let f = False in t",
    "let not = λx. let t = True in let f = False in x f t in not",
    "let zero = 0 in let succ = λn. n in succ zero",
    "let id = λx. x in id (id (id 42))",
    "let comp = λf. λg. λx. f (g x) in let id = λx. x in comp id id 42",
    "let flip = λf. λx. λy. f y x in let k = λx. λy. x in flip k 1 2"
  ]

main :: IO ()
main = do
  forM_ tests $ \test -> do
    putStrLn $ "> " ++ test
    putStr "= "
    case runP test of
      Left err -> print err
      Right term ->
        case runInfer term of
          Left err -> print err
          Right typ -> print typ

    putStrLn $ replicate 40 '-'
