{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Ast
import Control.Monad.Trans
import Data.List (span, union, (\\))
import qualified Data.Map as Map
import Data.Text (Text, pack)

data Sort = Prop | Type Int deriving (Show, Eq)

data Term
  = Sort Sort
  | Var Ast.Var
  | App Term Term
  | Lambda Ast.Var Term Term
  | ForAll Ast.Var Term Term
  deriving (Show)

instance Eq Term where
  (==) = alphaEq []
   where
    alphaEq :: [(Ast.Var, Ast.Var)] -> Term -> Term -> Bool
    alphaEq _ (Sort s) (Sort s') = s == s'
    alphaEq env (Var v) (Var v') = eqVar env v v'
    alphaEq env (App l r) (App l' r') = alphaEq env l l' && alphaEq env r r'
    alphaEq env (Lambda v t b) (Lambda v' t' b') = alphaEq env t t' && alphaEq ((v, v') : env) b b'
    alphaEq env (ForAll v t b) (ForAll v' t' b') = alphaEq env t t' && alphaEq ((v, v') : env) b b'
    alphaEq _ _ _ = False

    eqVar [] l r = l == r
    eqVar ((x, y) : env) l r = x == l && y == r || x /= l && y /= r && eqVar env l r

normalize :: Term -> Term
normalize (App f a) = case f' of
  Lambda v tp body -> normalize (subst v a body)
  _ -> App f' a'
 where
  f' = normalize f
  a' = normalize a
normalize (Lambda v t t') = Lambda v (normalize t) (normalize t')
normalize (ForAll v t t') = ForAll v (normalize t) (normalize t')
normalize (Sort s) = Sort s
normalize (Var v) = Var v

freeVars :: Term -> [Ast.Var]
freeVars (Sort _) = []
freeVars (Var x) = [x]
freeVars (Lambda v t b) = freeVars t `union` (freeVars b \\ [v])
freeVars (ForAll v t b) = freeVars t `union` (freeVars b \\ [v])
freeVars (App a b) = freeVars a `union` freeVars b

allVars :: Term -> [Ast.Var]
allVars (Sort _) = []
allVars (Var x) = [x]
allVars (Lambda v t b) = allVars t `union` allVars b
allVars (ForAll v t b) = allVars t `union` allVars b
allVars (App a b) = allVars a `union` allVars b

subst :: Ast.Var -> Term -> Term -> Term
subst x s b = sub vs0 b
 where
  sub _ s@(Sort _) = s
  sub _ e@(Var v)
    | v == x = s
    | otherwise = e
  sub vs e@(Lambda v t e')
    | v == x = e
    | v `elem` fvs = Lambda v' t' (sub (v' : vs) e'')
    | otherwise = Lambda v t' (sub vs e')
   where
    v' = newId vs
    e'' = subst v (Var v') e'
    t' = sub vs t
  sub vs e@(ForAll v t e')
    | v == x = e
    | v `elem` fvs = ForAll v' t' (sub (v' : vs) e'')
    | otherwise = ForAll v t' (sub vs e')
   where
    v' = newId vs
    e'' = subst v (Var v') e'
    t' = sub vs t
  sub vs (App f a) = sub vs f `App` sub vs a
  fvs = freeVars s
  vs0 = fvs `union` allVars b

newId :: [Ast.Var] -> Ast.Var
newId vs = head (fmap (Ast.V . pack) names \\ vs)

names :: [String]
names = [[i] | i <- ['a' .. 'z']] ++ [i : show j | j <- [1 ..], i <- ['a' .. 'z']]