module Main where

import Control.Monad (forM_)
import Interp
import Parser (runP)

tests :: [String]
tests =
  [ "(λx. λy. x) (λx. x)"
  ]

main :: IO ()
main = do
  forM_ tests $ \test -> do
    case runP test of
      Left err -> putStrLn $ "Error: " ++ show err
      Right term -> do
        let ctx = []
        putStrLn $ "Parsed term: " ++ show term
        let bruijn = removeNames term ctx
        putStrLn $ "Bruijn form: " ++ show bruijn
        let evaluated = eval bruijn
        putStrLn $ "Evaluated Bruijn form: " ++ show evaluated
        putStrLn $ "Final term: " ++ show (restoreNames evaluated ctx)
    putStrLn $ replicate 40 '-'
