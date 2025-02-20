module Tree where

type Name = String

-- τ ::= ()       (Unit type)
--     | Nat      (Natual numbers)
--     | α        (Type var)
--     | τ -> τ   (Arrow type)
--     | ∀α. τ    (Arrow type)
data Typ
  = TUnit
  | TNat
  | TVar Name
  | TArrow Typ Typ
  | TForall Name Typ
  deriving (Eq)

instance Show Typ where
  show TUnit = "Unit"
  show TNat = "Nat"
  show (TVar name) = name
  show (TArrow t1 t2) =
    case t1 of
      TArrow {} -> "(" ++ show t1 ++ ") -> " ++ show t2
      _ -> show t1 ++ " -> " ++ show t2
  show (TForall name typ) =
    "∀" ++ name ++ ". " ++ show typ

-- e ::=
--     | ()        (Unit)
--     | 0         (Zero)
--     | Succ e    (Succ)
--     | x         (Variable)
--     | λx:τ. e   (Abstraction)
--     | e e       (Application)
--     | Λα. e     (Type abstraction)
--     | e [τ]     (Type application)
data Term
  = TmUnit
  | TmZero
  | TmSucc Term
  | TmVar Name
  | TmAbs Name Typ Term
  | TmApp Term Term
  | TmTAbs Name Term
  | TmTApp Term Typ

instance Show Term where
  show TmUnit = "()"
  show TmZero = "0"
  show (TmSucc t) = "succ " ++ show t
  show (TmVar name) = name
  show (TmAbs param t body) = "λ" ++ param ++ ":" ++ show t ++ ". " ++ show body
  show (TmApp func argm) = case func of
    TmAbs {} -> "(" ++ show func ++ ") " ++ show argm
    _ -> show func ++ " " ++ show argm
  show (TmTAbs param body) = "Λ" ++ param ++ ". " ++ show body
  show (TmTApp t t') = show t ++ " [" ++ show t' ++ "]"
