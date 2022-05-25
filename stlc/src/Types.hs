module Types where

newtype Ident = Ident String
  deriving (Eq, Ord, Show)

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
  | TmVar Ident
  | TmAbs Ident Typ Term
  | TmApp Term Term
  | TmIf Term Term Term
  deriving (Eq, Show)

data TypError
  = TyMismatch Typ Typ
  | TyExpectedFunction Typ
  | TyUnboundVar Ident
  deriving (Eq, Show)
