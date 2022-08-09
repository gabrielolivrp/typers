module Syntax where

type Name = String

-- e ::= l                  (literals)
--     | x                  (variable)
--     | λx. e              (abstraction)
--     | e1 e2              (application)
--     | let x = e1 in e2   (let)
data Term
  = TmLit Literal
  | TmVar Name
  | TmAbs Name Term
  | TmApp Term Term
  | TmLet Name Term Term
  deriving (Show)

-- l ::= n    (numbers)
--     | b    (booleans)
data Literal
  = LInt Int
  | LBool Bool
  deriving (Show)

newtype TVar = MkTVar String
  deriving (Show, Ord, Eq)

-- Mono types
-- τ ::= α          Variable
--     | C τ...τ  Constants
--     | τ -> τ     Arrow
data Typ
  = TCon String
  | TVar TVar
  | TArrow Typ Typ
  deriving (Eq, Ord, Show)

-- Poly types
-- σ ::= τ        (mono type)
--     | ∀ α̅.τ    (quantifier)
data Scheme = Forall [TVar] Typ
  deriving (Eq, Ord, Show)
