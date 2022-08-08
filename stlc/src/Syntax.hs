module Syntax where

type Name = String

-- τ ::= T        (Type)
--     | τ → τ    (Arrow type)
data Typ
  = TUnit
  | TInt
  | TArrow Typ Typ
  deriving (Eq, Show)

-- e ::= C         (Constant)
--     | x         (Variable)
--     | λx:τ. e   (Abstraction)
--     | e e       (Application)
data Term
  = TmUnit
  | TmInt Int
  | TmVar String
  | TmAbs String Typ Term
  | TmApp Term Term
  deriving (Eq, Show)
