{-# LANGUAGE ImportQualifiedPost #-}

module Typer where

import Data.Map qualified as M
import Data.Set qualified as S
import Tree

-- Context type
-- Γ ::= ϵ          (empty)
--     | Γ, x : τ
type Context = M.Map Name Typ

data TypeError
  = MismatchedTypes Typ Typ
  | ExpectedFunctionType Typ
  | ExpectedForallType Typ
  | UndefinedVariable String
  deriving (Eq)

instance Show TypeError where
  show (MismatchedTypes t1 t2) = "Mismatched types: expected " ++ show t1 ++ ", found " ++ show t2
  show (ExpectedFunctionType t) = "Expected function type, found " ++ show t
  show (UndefinedVariable x) = "Undefined variable: " ++ x
  show (ExpectedForallType t) = "Expected forall type, found " ++ show t

emptyContext :: Context
emptyContext = M.empty

infer :: Context -> Term -> Either TypeError Typ
--
-- ------------- (Unit)
-- Γ ⊢ () : Unit
infer _ TmUnit = pure TUnit
--
-- ------------ (Zero)
-- Γ ⊢ Zero : Nat
infer _ TmZero = pure TNat
--
--  Γ ⊢ t : Nat
-- -------------- (Succ)
-- Γ ⊢ Succ t : Nat
infer ctx (TmSucc t) = do
  t' <- infer ctx t
  if t' == TNat
    then pure TNat
    else Left (MismatchedTypes TNat t')
--
--  x:τ ∈ Γ
-- --------- (Var)
--  Γ ⊢ x : τ
infer ctx (TmVar name) =
  case M.lookup name ctx of
    Just typ -> pure typ
    Nothing -> Left (UndefinedVariable name)
--
--  Γ, x:τ ⊢ body : T
-- ------------------------ (Abs)
-- Γ ⊢ λx:τ. body : τ -> T
infer ctx (TmAbs param t body) = do
  let ctx' = M.insert param t ctx
  bodyT <- infer ctx' body
  pure (TArrow t bodyT)
--
--  Γ ⊢ func : τ -> T     Γ ⊢ argm : τ
-- ------------------------------------- (App)
--         Γ ⊢ func argm : T
infer ctx (TmApp func argm) = do
  funcT <- infer ctx func
  argmT <- infer ctx argm
  case funcT of
    TArrow param body | alphaEq param argmT -> pure body
    TArrow param _ -> Left (MismatchedTypes param argmT)
    _ -> Left (ExpectedFunctionType funcT)
--
-- Γ ⊢ α type ⊢ body : T
-- ------------------------ (TAbs)
-- Γ ⊢ Λα. body : ∀α. T
infer ctx (TmTAbs param body) = do
  let ctx' = M.insert param (TVar param) ctx
  t <- infer ctx' body
  pure (TForall param t)

--
--  Γ M: ∀α. T
-- --------------------- (TApp)
--  Γ ⊢ M[τ] : T[τ/α]
infer ctx (TmTApp func argm) = do
  funcT <- infer ctx func
  case funcT of
    TForall param body -> pure (subst body param argm)
    _ -> Left (ExpectedForallType funcT)

subst :: Typ -> Name -> Typ -> Typ
subst tvar@(TVar name) from to
  | name == from = to
  | otherwise = tvar
subst (TForall param body) from to
  | param == from = TForall param body
  | otherwise = TForall param (subst body from to)
subst (TArrow param body) from to =
  let param' = subst param from to
      body' = subst body from to
   in TArrow param' body'
subst typ _ _ = typ

alphaEq :: Typ -> Typ -> Bool
alphaEq TNat TNat = True
alphaEq TUnit TUnit = True
alphaEq (TVar _) (TVar _) = True
alphaEq (TArrow param body) (TArrow param' body') =
  alphaEq param param' && alphaEq body body'
alphaEq (TForall param body) (TForall param' body') =
  let body'' = subst body' param' (TVar param)
   in alphaEq body body''
alphaEq _ _ = False
