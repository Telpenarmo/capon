{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (
  Term (..),
  Env (..),
  Sort (..),
  LData (..),
  FData (..),
  Var,
  normalize,
  substitute,
  emptyEnv,
  extend,
  var,
  name,
) where

import qualified Ast
import qualified Data.Map as Map
import Data.Text (Text, unpack)

data Var = V Text Int deriving (Eq, Ord)
name :: Var -> Text
name (V x _) = x
var :: Text -> Var
var x = V x 0

instance Show Var where
  show (V x n) = unpack x ++ replicate n '^'

data Sort = Prop | Type Int deriving (Show, Eq)

data LData = LD Text Term Term
data FData = FD Text Term Term

data Term
  = Sort Sort
  | Var Var
  | App Term Term
  | Lambda LData
  | ForAll FData

instance Eq Term where
  (==) = alpha []
   where
    alpha _ (Sort s) (Sort s') = s == s'
    alpha ctx (Var v) (Var v') = varEq ctx v v'
    alpha ctx (App f a) (App f' a') = alpha ctx f f' && alpha ctx a a'
    alpha ctx (Lambda (LD x tp bd)) (Lambda (LD x' tp' bd')) =
      alpha ctx tp tp' && alpha ((x, x') : ctx) bd bd'
    alpha ctx (ForAll (FD x tp bd)) (ForAll (FD x' tp' bd')) =
      alpha ctx tp tp' && alpha ((x, x') : ctx) bd bd'
    alpha _ _ _ = False

    varEq [] (V x n) (V x' n') = x == x' && n == n'
    varEq ((xL', xR') : ctx) (V xL nL) (V xR nR) =
      xL == xL' && xR == xR' || varEq ctx (V xL nL') (V xR nR')
     where
      !nL' = if xL == xL' then nL - 1 else nL
      !nR' = if xR == xR' then nR - 1 else nR

normalize :: Term -> Term
normalize (App f a) = case normalize f of
  Lambda (LD x tp body) -> normalize $ substitute x a body
  f' -> App f' $ normalize a
normalize (Lambda (LD v t bd)) = Lambda $ LD v (normalize t) (normalize bd)
normalize (ForAll (FD v t bd)) = ForAll $ FD v (normalize t) (normalize bd)
normalize (Sort s) = Sort s
normalize (Var v) = Var v

subst :: Text -> Int -> Term -> Term -> Term
subst x n arg = \case
  (Sort s) -> Sort s
  e@(Var (V x' n')) -> if x == x' && n == n' then arg else e
  App f a -> App (go f) (go a)
  Lambda (LD x' tp bd) -> Lambda $ LD x' tp' bd'
   where
    tp' = go tp
    bd' = subst x n' (shift 1 x' arg) bd
    !n' = if x == x' then n + 1 else n
  ForAll (FD x' tp bd) -> ForAll $ FD x' tp' bd'
   where
    tp' = go tp
    bd' = subst x n' (shift 1 x' arg) bd
    !n' = if x == x' then n + 1 else n
 where
  go = subst x n arg

substitute :: Text -> Term -> Term -> Term
substitute x arg = shift (-1) x . subst x 0 (shift 1 x arg)

newtype Env = Env (Map.Map Text Term)

emptyEnv :: Env
emptyEnv = Env Map.empty
extend :: Env -> (Text, Term) -> Env
extend (Env env) (x, s) = Env (shift 1 x <$> Map.insert x s env)
lookupEnv :: Env -> Text -> Maybe Term
lookupEnv (Env env) v = Map.lookup v env

shift :: Int -> Text -> Term -> Term
shift d x e = go e 0
 where
  go e lev = case e of
    Lambda (LD x' tp bd) -> Lambda $ LD x' (go tp lev') (go bd lev)
     where
      !lev' = if x == x' then lev + 1 else lev
    ForAll (FD x' tp bd) -> ForAll $ FD x' (go tp c') (go bd lev)
     where
      !c' = if x == x' then lev + 1 else lev
    App f a -> App (go f lev) (go a lev)
    Var (V x' n) -> Var $ V x' n'
     where
      !n' = if x == x' && n >= lev then n + d else n
    Sort s -> Sort s