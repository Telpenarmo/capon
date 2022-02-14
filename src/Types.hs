{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Ast
import Control.Monad.Trans
import Data.List (span, union, (\\))
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)

type Var = Text
data Sort = Prop | Type Int deriving (Show, Eq)

type Abstraction = (Var, Term, Term)
newtype LData = LD Abstraction
newtype FData = FD Abstraction

data Term
  = Sort Sort
  | Var Var
  | App Term Term
  | Lambda LData
  | ForAll FData

instance Eq Term where
  (==) = alphaEq []
   where
    alphaEq :: [(Var, Var)] -> Term -> Term -> Bool
    alphaEq _ (Sort s) (Sort s') = s == s'
    alphaEq env (Var v) (Var v') = eqVar env v v'
    alphaEq env (App l r) (App l' r') = alphaEq env l l' && alphaEq env r r'
    alphaEq env (Lambda (LD (v, t, b))) (Lambda (LD (v', t', b'))) = alphaEq env t t' && alphaEq ((v, v') : env) b b'
    alphaEq env (ForAll (FD (v, t, b))) (ForAll (FD (v', t', b'))) = alphaEq env t t' && alphaEq ((v, v') : env) b b'
    alphaEq _ _ _ = False

    eqVar [] l r = l == r
    eqVar ((x, y) : env) l r = x == l && y == r || x /= l && y /= r && eqVar env l r

normalize :: Term -> Term
normalize (App f a) = case f' of
  Lambda (LD (v, tp, body)) -> normalize (subst v a body)
  _ -> App f' a'
 where
  f' = normalize f
  a' = normalize a
normalize (Lambda (LD (v, t, t'))) = Lambda $ LD (v, normalize t, normalize t')
normalize (ForAll (FD (v, t, t'))) = ForAll $ FD (v, normalize t, normalize t')
normalize (Sort s) = Sort s
normalize (Var v) = Var v

freeVars :: Term -> [Var]
freeVars (Sort _) = []
freeVars (Var x) = [x]
freeVars (Lambda (LD (v, t, b))) = freeVars t `union` (freeVars b \\ [v])
freeVars (ForAll (FD (v, t, b))) = freeVars t `union` (freeVars b \\ [v])
freeVars (App a b) = freeVars a `union` freeVars b

allVars :: Term -> [Var]
allVars (Sort _) = []
allVars (Var x) = [x]
allVars (Lambda (LD (v, t, b))) = allVars t `union` allVars b
allVars (ForAll (FD (v, t, b))) = allVars t `union` allVars b
allVars (App a b) = allVars a `union` allVars b

subst :: Var -> Term -> Term -> Term
subst x s b = sub vs0 b
 where
  sub _ s@(Sort _) = s
  sub _ e@(Var v)
    | v == x = s
    | otherwise = e
  sub vs e@(Lambda (LD (v, t, e')))
    | v == x = e
    | v `elem` fvs = Lambda $ LD (v', t', sub (v' : vs) e'')
    | otherwise = Lambda $ LD (v, t', sub vs e')
   where
    v' = newId vs
    e'' = subst v (Var v') e'
    t' = sub vs t
  sub vs e@(ForAll (FD (v, t, e')))
    | v == x = e
    | v `elem` fvs = ForAll $ FD (v', t', sub (v' : vs) e'')
    | otherwise = ForAll $ FD (v, t', sub vs e')
   where
    v' = newId vs
    e'' = subst v (Var v') e'
    t' = sub vs t
  sub vs (App f a) = sub vs f `App` sub vs a
  fvs = freeVars s
  vs0 = fvs `union` allVars b

newId :: [Var] -> Var
newId vs = head (fmap pack names \\ vs)

names :: [String]
names = [[i] | i <- ['a' .. 'z']] ++ [i : show j | j <- [1 ..], i <- ['a' .. 'z']]

newtype Env = Env (Map.Map Var Term)

emptyEnv :: Env
emptyEnv = Env Map.empty
extend :: Env -> (Var, Term) -> Env
extend (Env env) (x, s) = Env $ Map.insert x s env
unionEnv :: Env -> Env -> Env
unionEnv (Env l) (Env r) = Env $ Map.union l r
singleton :: Var -> Term -> Env
singleton v t = Env $ Map.singleton v t
lookupEnv :: Env -> Var -> Maybe Term
lookupEnv (Env env) v = Map.lookup v env
subset :: Env -> Env -> Bool
subset (Env l) (Env r) = Map.empty == Map.difference l r