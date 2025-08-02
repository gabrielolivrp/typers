module Interp where

import Tree

type NameContext = [Name]

index :: (Eq a) => [a] -> a -> Idx
index [] _ = error "Element not found in context"
index (x : xs) y
  | x == y = 0
  | otherwise = 1 + index xs y

removeNames :: Term -> NameContext -> Bruijn
removeNames (TmVar name) ctx = BVar (index ctx name)
removeNames (TmAbs param body) ctx = BAbs (removeNames body (param : ctx))
removeNames (TmApp f a) ctx = BApp (removeNames f ctx) (removeNames a ctx)

restoreNames :: Bruijn -> NameContext -> Term
restoreNames (BVar (Idx i)) ctx = TmVar (ctx !! i)
restoreNames (BAbs body) ctx =
  let param = pickFreshName ctx (Name "x")
   in TmAbs param (restoreNames body (param : ctx))
restoreNames (BApp f a) ctx =
  TmApp (restoreNames f ctx) (restoreNames a ctx)

pickFreshName :: NameContext -> Name -> Name
pickFreshName ctx (Name name) = go name 1
 where
  go :: String -> Int -> Name
  go b i =
    let n = Name b
     in if n `elem` ctx
          then go (b ++ show i) (i + 1)
          else n

shift :: Int -> Bruijn -> Bruijn
shift offset = walk 0
 where
  walk d (BVar (Idx i))
    | i >= d = mkVar (i + offset)
    | otherwise = mkVar i
  walk d (BAbs body) = BAbs (walk (d + 1) body)
  walk d (BApp funct argum) = BApp (walk d funct) (walk d argum)

subst :: Int -> Bruijn -> Bruijn -> Bruijn
subst j term = walk 0
 where
  walk d (BVar (Idx i))
    | i == j + d = shift d term
    | otherwise = mkVar i
  walk d (BAbs body) = BAbs (walk (d + 1) body)
  walk d (BApp funct argum) = BApp (walk d funct) (walk d argum)

eval :: Bruijn -> Bruijn
eval (BApp (BAbs body) argum) =
  shift (-1) (subst 0 (shift 1 argum) body)
eval (BApp funct argum) =
  let funct' = eval funct
      argum' = eval argum
   in eval (BApp funct' argum')
eval t = t
