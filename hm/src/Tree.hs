module Tree where

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

instance Show Term where
  show (TmLit l) = show l
  show (TmVar x) = x
  show (TmAbs x e) = "λ" ++ x ++ ". " ++ show e
  show (TmApp e1 e2) = show e1 ++ " " ++ show e2
  show (TmLet x e1 e2) = "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2

-- l ::= n    (numbers)
--     | b    (booleans)
data Literal
  = LInt Int
  | LBool Bool

instance Show Literal where
  show (LInt n) = show n
  show (LBool b) = show b

newtype TVar = MkTVar String
  deriving (Show, Ord, Eq)

-- Mono types
-- τ ::= α          Variable
--     | C τ...τ    Constants
--     | τ -> τ     Arrow
data Typ
  = TVar TVar
  | TCon String
  | TArrow Typ Typ
  deriving (Eq, Ord)

instance Show Typ where
  show (TVar (MkTVar a)) = a
  show (TCon c) = c
  show (TArrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

-- Poly types
-- σ ::= τ      (mono type)
--     | ∀ α̅.τ  (quantifier)
data Scheme = Forall [TVar] Typ
  deriving (Eq, Ord)

instance Show Scheme where
  show (Forall [] t) = show t
  show (Forall as t) = "∀" ++ unwords (map show as) ++ ". " ++ show t
