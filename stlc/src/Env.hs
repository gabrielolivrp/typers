module Env where

import qualified Data.Map as M

newtype Env k v = Env (M.Map k v)
  deriving (Show)

empty :: Env k v
empty = Env M.empty

lookup :: Ord k => k -> Env k v -> Maybe v
lookup k (Env env) = M.lookup k env

extend :: Ord k => k -> v -> Env k v -> Env k v
extend k v (Env env) = Env (M.insert k v env)
