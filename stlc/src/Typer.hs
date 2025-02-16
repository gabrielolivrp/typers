{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Typer where

import Data.Map qualified as M
import Tree

-- Context type
-- Γ ::= ϵ          (empty)
--     | Γ, x : σ
type Context = M.Map String Typ

-- Type errors
data TypeError
  = MismatchedTypes Typ Typ
  | ExpectedFunctionType Typ
  | UndefinedVariable String
  deriving (Eq)

instance Show TypeError where
  show = \case
    MismatchedTypes t1 t2 ->
      "Mismatched types: expected " ++ show t1 ++ ", found " ++ show t2
    ExpectedFunctionType t ->
      "Expected function type, found " ++ show t
    UndefinedVariable x ->
      "Undefined variable: " ++ x

emptyContext :: Context
emptyContext = M.empty

typeCheck :: Context -> Term -> Either TypeError Typ
typeCheck ctx = \case
  --
  -- -------------- (Unit)
  --  Γ ⊢ (): Unit
  TmUnit -> pure TUnit
  --
  -- Zero: Nat
  -- ------------ (Zero)
  -- Γ ⊢ Zero: Nat
  --
  TmZero -> pure TNat
  --
  --  Γ ⊢ t: Nat
  -- ---------------- (Succ)
  -- Γ ⊢ Succ t: Nat
  --
  TmSucc t -> do
    t' <- typeCheck ctx t
    if t' == TNat
      then pure TNat
      else Left (MismatchedTypes TNat t')
  --
  --  x:σ ∈ Γ
  -- --------- (Var)
  --  Γ ⊢ x:σ
  --
  TmVar x ->
    case M.lookup x ctx of
      Just t -> pure t
      Nothing -> Left (UndefinedVariable x)
  --
  --       Γ, param:σ ⊢ body:τ
  -- ------------------------------- (Abs)
  --  Γ ⊢ (λparam:σ. body): (σ -> τ)
  --
  TmAbs param t body -> do
    let ctx' = M.insert param t ctx
    bodyT <- typeCheck ctx' body
    pure (TArrow t bodyT)
  --
  --   Γ ⊢ func:σ → τ   Γ ⊢ argm:σ
  -- ------------------------------ (App)
  --      Γ ⊢ func argm:τ
  --
  TmApp func argm -> do
    funcT <- typeCheck ctx func
    argmT <- typeCheck ctx argm
    case funcT of
      TArrow paramT bodyT ->
        if paramT == argmT
          then pure bodyT
          else Left (MismatchedTypes paramT argmT)
      _ -> Left (ExpectedFunctionType funcT)
