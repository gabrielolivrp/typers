{-# LANGUAGE InstanceSigs #-}

module Infer where

import qualified Data.Map as M
import Syntax

-- Context type
-- Γ ::= ϵ          (empty)
--     | Γ, x : τ
type Context = M.Map Name Typ

data TypeError
  = TypeMismatch String Typ
  | ExpectedFunction Typ
  | UnboundVar Name
  deriving (Eq, Show)

instance Eq Typ where
  TInt == TInt = True
  TUnit == TUnit = True
  TVar name == TVar name' = name == name'
  TArrow ptyp btyp == TArrow ptyp' btyp' =
    ptyp == ptyp' && btyp == btyp'
  tforall@(TForall param btyp) == TForall param' btyp' =
    tforall == subst btyp' param' (TVar param)
  _ == _ = False

emptyContext :: Context
emptyContext = M.empty

infer :: Context -> Term -> Either TypeError Typ
infer ctx (Var name) =
  case M.lookup name ctx of
    Just typ -> return typ
    Nothing -> Left (UnboundVar name)
infer ctx (Abs param typ body) = do
  let ctx' = M.insert param typ ctx
  btyp <- infer ctx' body
  return (TArrow typ btyp)
infer ctx (App func argm) = do
  ftyp <- infer ctx func
  atyp <- infer ctx func
  case ftyp of
    TArrow ptyp btyp ->
      if ptyp == atyp
        then return btyp
        else Left (TypeMismatch (show ptyp) atyp)
    _ -> Left (ExpectedFunction ftyp)
infer ctx (TAbs param body) =
  case infer ctx body of
    Right typ -> return (TForall param typ)
    Left err -> Left err
infer ctx (TApp func argm) = do
  ftyp <- infer ctx func
  case ftyp of
    TForall tvar typ ->
      return (subst typ tvar argm)
    typ -> Left (TypeMismatch "forall" typ)

subst :: Typ -> Name -> Typ -> Typ
subst tvar@(TVar name) from to
  | name == from = to
  | otherwise = tvar
subst (TForall param btyp) from to
  | param == from = TForall param btyp
  | otherwise = TForall param (subst btyp from to)
subst (TArrow ptyp btyp) from to =
  let ptyp' = subst ptyp from to
      btyp' = subst btyp from to
   in TArrow ptyp' btyp'
subst typ _ _ = typ
