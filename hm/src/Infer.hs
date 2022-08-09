module Infer where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Syntax

-- References
-- https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.18.9348&rep=rep1&type=pdf
-- https://blog.stimsina.com/post/implementing-a-hindley-milner-type-system-part-2
-- https://github.com/sdiehl/write-you-a-haskell/blob/master/006_hindley_milner.md

-- Unique value
newtype Unique = Unique
  { count :: Int
  }

-- Context type
-- Γ ::= ϵ          (empty)
--     | Γ, x : σ
newtype Context = Context (M.Map String Scheme)

-- Substitution
type Subst = M.Map TVar Typ

-- Type errors
data TypeError
  = UnboundVariable String
  | UnificationFail
  | InfiniteType
  deriving (Show)

-- Monad to inference
type InferM a = ExceptT TypeError (State Unique) a

class Apply a where
  apply :: Subst -> a -> a
  ftv :: a -> S.Set TVar

emptySubst :: Subst
emptySubst = M.empty

-- Context functions
emptyContext :: Context
emptyContext = Context M.empty

extend :: String -> Scheme -> Context -> Context
extend k a (Context ctx) = Context $ M.insert k a ctx

-- Instances for substitution types

instance Apply Typ where
  -- [C/s] = C
  apply _ c@TCon {} = c
  -- [x/s]x = s
  -- [x/s]y = y   x ≠ y
  apply s t@(TVar y) = M.findWithDefault t y s
  -- [x/s](t1 -> t2) = [x/s]t1 -> [x/s]t2
  apply s (TArrow t1 t2) = TArrow (apply s t1) (apply s t2)

  -- FTV(C) = ∅
  ftv TCon {} = S.empty
  -- FTV(σ) = σ
  ftv (TVar s) = S.singleton s
  -- FTV(τ1 → τ2) = FTV(τ1) ∪ FTV(τ2)
  ftv (TArrow t1 t2) = S.union (ftv t1) (ftv t2)

instance Apply Scheme where
  -- [x/s] ∀a.t = ∀a. [x/s]t   s ≠ a
  apply s (Forall as t) = Forall as $ apply (foldr M.delete s as) t

  -- FTV(∀x.t) = FTV(t) - x
  ftv (Forall x t) = S.difference (ftv t) (S.fromList x)

instance Apply Context where
  -- [t/s] Γ = y:[t/s]σ | y:σ ∈ Γ
  apply subst (Context ctx) = Context $ M.map (apply subst) ctx

  -- FTV(Γ) = FTV(y1) ∪ ... ∪ FTV(yn)
  ftv (Context ctx) = foldr (S.union . ftv) S.empty ctx

-- (S ◦ U) = S (U t)
compose :: Subst -> Subst -> Subst
compose s1 s2 = M.map (apply s1) (M.union s1 s2)

mgu :: Typ -> Typ -> InferM Subst
--
--   τ1 ~ τ1':S1   [S1]τ2 ~ [S1]τ2' (S2)
-- --------------------------------------- (Arrow)
--    τ1 → τ2 ~ τ1' → τ2': (s2 ◦ s1)
mgu (TArrow t1 t2) (TArrow t1' t2') = do
  s1 <- mgu t1 t1'
  s2 <- mgu (apply s1 t2) (apply s1 t2')
  pure (s1 `compose` s2)
--
--      α ∉ FTV(τ)
-- -------------------- (Var left)
--  α ~ τ: ([α |-> τ])
mgu (TVar a) t = bind a t
--
--      α ∉ FTV(τ)
-- -------------------- (Var right)
--  τ ~ α: ([α |-> τ])
mgu t (TVar a) = bind a t
mgu (TCon a) (TCon b)
  -- α ~ α: ∅   (Same)
  | a == b = pure emptySubst
mgu _ _ = throwError UnificationFail

bind :: TVar -> Typ -> InferM Subst
bind a t
  -- α ~ α: ∅   (Same)
  | t == TVar a = pure emptySubst
  | a `S.member` ftv t = throwError InfiniteType
  | otherwise = pure $ M.singleton a t

-- generalize(Γ, τ) = ∀α'.τ where α' = ftv(τ) − ftv(Γ)
gen :: Context -> Typ -> Scheme
gen ctx t = Forall as t
  where
    as = S.toList (ftv t `S.difference` ftv ctx)

-- instantiate(∀α1 ...αn.τ) = [α1 := β1,...,αn := βn]τ where β1,...,βn are fresh
inst :: Scheme -> InferM Typ
inst (Forall as t) = do
  as' <- mapM (const newvar) as
  let s = M.fromList $ zip as as'
  pure (apply s t)

infer :: Context -> Term -> InferM (Subst, Typ)
--
-- ------------ Int
--  Γ ⊢ n:Int,∅
infer _ (TmLit LInt {}) = pure (emptySubst, TCon "Int")
--
-- ------------------ Bool   ------------------- Bool
--  Γ ⊢ False:Bool,∅           Γ ⊢ True:Bool,∅
infer _ (TmLit LBool {}) = pure (emptySubst, TCon "Bool")
--
--  x:σ ∈ Γ     τ = inst(σ)
-- -------------------------- (Var)
--      Γ ⊢ x:τ,∅
infer (Context ctx) (TmVar x) =
  case M.lookup x ctx of
    Nothing -> throwError (UnboundVariable x)
    Just scheme -> do
      t <- inst scheme
      pure (emptySubst, t)
--
--  τ = newvar    Γ, param: τ ⊢ body:τ',S
-- --------------------------------------- (App)
--    Γ ⊢ λparam. body: Sτ → τ',S
infer ctx (TmAbs param body) = do
  t <- newvar
  let ctx' = extend param (Forall [] t) ctx
  (s, t') <- infer ctx' body
  pure (s, apply s (TArrow t t'))
--
--  Γ ⊢ func:τ0,S0           S0 Γ ⊢ argm:τ1,S1
--  τ' = newvar              S2 = mgu(S1 τ0, τ1 -> τ')
-- ---------------------------------------------------- (Abs)
--           Γ ⊢ func argm : S2 τ', S2 S1 S0
infer ctx (TmApp func argm) = do
  t <- newvar
  (s0, t0) <- infer ctx func
  (s1, t1) <- infer (apply s0 ctx) argm
  s2 <- mgu (apply s1 t0) (TArrow t t1)
  pure (s0 `compose` s1 `compose` s2, apply s2 t)
--                          ____
--  Γ e0:τ,S0      S0 Γ, x: S0 Γ(τ) ⊢ e1:τ',S1
-- --------------------------------------------- (Let)
--       Γ ⊢ let x = e0 in e1 : τ',S1 S0
infer ctx (TmLet x e0 e1) = do
  (s0, t) <- infer ctx e0
  let ctx' = apply s0 ctx
  let t' = gen ctx' t
  (s1, t') <- infer (extend x t' ctx') e1
  return (s0 `compose` s1, t')

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

newvar :: InferM Typ
newvar = do
  s <- get
  put s {count = count s + 1}
  return $ TVar $ MkTVar (letters !! count s)

runInfer :: InferM (Subst, Typ) -> Either TypeError Typ
runInfer m = case evalState (runExceptT m) (Unique 0) of
  Left err -> Left err
  Right typ -> (Right . snd) typ
