module Types where

type Name = String

data Typ
  = TyBool
  | TyUnit
  | TyInt
  | TyArrow Typ Typ
  deriving (Eq, Show)

data Term
  = TmUnit
  | TmTrue
  | TmFalse
  | TmInt Int
  | TmVar Name
  | TmAbs Name Typ Term
  | TmApp Term Term
  | TmIf Term Term Term
  deriving (Eq, Show)

data TypError
  = TyMismatch Typ Typ
  | TyExpectedFunction Typ
  | TyUnboundVar Name
  deriving (Eq, Show)
