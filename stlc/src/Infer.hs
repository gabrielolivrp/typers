{-# LANGUAGE LambdaCase #-}

module Infer where

import qualified Data.Map as M
import Syntax

-- Context type
-- Γ ::= ϵ          (empty)
--     | Γ, x : σ
type Context = M.Map String Typ

-- Type errors
data TypeError
  = TypeMismatch Typ Typ
  | ExpectedFunction Typ
  | UnboundVar String
  deriving (Eq, Show)

emptyContext :: Context
emptyContext = M.empty

infer :: Context -> Term -> Either TypeError Typ
infer ctx = \case
  --
  --  C is a constante of type T
  -- ---------------------------- (Constant)
  --        Γ ⊢ c: T
  --
  TmUnit -> return TUnit
  TmInt {} -> return TInt
  --
  --  x:σ ∈ Γ
  -- --------- (Var)
  --  Γ ⊢ x:σ
  --
  TmVar x ->
    case M.lookup x ctx of
      Just typ -> return typ
      Nothing -> Left (UnboundVar x)
  --
  --       Γ, param:σ ⊢ body:τ
  -- ------------------------------- (Abs)
  --  Γ ⊢ (λparam:σ. body): (σ → τ)
  --
  TmAbs param typ body -> do
    let ctx' = M.insert param typ ctx
    bodyTyp <- infer ctx' body
    return (TArrow typ bodyTyp)
  --
  --   Γ ⊢ func:σ → τ   Γ ⊢ argm:σ
  -- ------------------------------ (App)
  --      Γ ⊢ func argm:τ
  --
  TmApp func argm -> do
    funcTyp <- infer ctx func
    argmTyp <- infer ctx argm
    case funcTyp of
      TArrow paramTyp bodyTyp ->
        if paramTyp == argmTyp
          then return bodyTyp
          else Left $ TypeMismatch paramTyp argmTyp
      _ -> Left $ ExpectedFunction funcTyp
