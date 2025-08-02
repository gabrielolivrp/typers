{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tree where

newtype Idx = Idx Int
  deriving (Eq, Num)

instance Show Idx where
  show (Idx i) = show i

data Bruijn
  = BVar Idx
  | BAbs Bruijn
  | BApp Bruijn Bruijn
  deriving (Eq)

instance Show Bruijn where
  show (BVar i) = show i
  show (BAbs t) = "λ." ++ show t
  show (BApp t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

mkVar :: Int -> Bruijn
mkVar i = BVar (Idx i)

newtype Name = Name String
  deriving (Ord, Eq)

instance Show Name where
  show (Name n) = n

data Term
  = TmVar Name
  | TmAbs Name Term
  | TmApp Term Term
  deriving (Eq)

instance Show Term where
  show (TmVar x) = show x
  show (TmAbs x t) = "λ" ++ show x ++ "." ++ show t
  show (TmApp t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
