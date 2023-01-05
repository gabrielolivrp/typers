module Syntax where

type Name = String

-- τ ::= ()       (Unit type)
--     | Int      (Int type)
--     | α        (Type var)
--     | τ → τ    (Arrow type)
--     | ∀α. τ    (Arrow type)
data Typ
  = TUnit
  | TInt
  | TVar Name
  | TArrow Typ Typ
  | TForall Name Typ
  deriving (Show)

-- e ::= C         (Constant)
--     | x         (Variable)
--     | λx:τ. e   (Abstraction)
--     | e e       (Application)
--     | Λα. e     (Type abstraction)
--     | e [τ]     (Type application)
data Term
  = Unit
  | Int Int
  | Var Name
  | Abs Name Typ Term
  | App Term Term
  | TAbs Name Term
  | TApp Term Typ
  deriving (Show)
