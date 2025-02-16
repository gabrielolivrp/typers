module Tree where

type Name = String

-- τ ::= Unit     (Unit type)
--     | Nat      (Natural numbers)
--     | τ → τ    (Function type)
data Typ
  = TNat
  | TUnit
  | TArrow Typ Typ
  deriving (Eq)

instance Show Typ where
  show TNat = "Nat"
  show TUnit = "Unit"
  show (TArrow t1 t2) =
    case t1 of
      TArrow {} -> "(" ++ show t1 ++ ") -> " ++ show t2
      _ -> show t1 ++ " -> " ++ show t2

-- e ::=
--     | ()        (Unit value)
--     | Zero      (Zero)
--     | Succ e    (Successor)
--     | x         (Variable)
--     | λx:τ. e   (Abstraction)
--     | e e       (Application)
data Term
  = TmUnit
  | TmZero
  | TmSucc Term
  | TmVar String
  | TmApp Term Term
  | TmAbs String Typ Term
  deriving (Eq)

instance Show Term where
  show TmUnit = "()"
  show TmZero = "0"
  show (TmSucc n) = show (1 + read (show n))
  show (TmVar x) = x
  show (TmAbs x t e) = "λ" ++ x ++ ": " ++ show t ++ ". " ++ show e
  show (TmApp e1 e2) =
    case e1 of
      TmAbs {} -> "(" ++ show e1 ++ ") " ++ show e2
      _ -> show e1 ++ " " ++ show e2
