{-# LANGUAGE OverloadedStrings #-}

module Typechecker (typecheck) where

import qualified Ast
import Control.Monad.Except
import qualified Control.Monad.Except as Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import Data.Text
import GHC.Real (reduce)
import qualified Types as Tp

type TypeError = Text

newtype Env = Env (Map.Map Ast.Var Tp.Term)

emptyEnv :: Env
emptyEnv = Env Map.empty
extend :: Env -> (Ast.Var, Tp.Term) -> Env
extend (Env env) (x, s) = Env $ Map.insert x s env

type Infer a = Except TypeError a

typecheck :: Ast.Expr -> Either TypeError Tp.Term
typecheck e = runInfer $ infer emptyEnv e

runInfer :: Infer Tp.Term -> Either TypeError Tp.Term
runInfer m = case runExcept m of
    Left err -> Left err
    Right res -> Right res

sortRule :: Ast.Sort -> Infer Tp.Term
sortRule Ast.Prop = return $ Tp.Sort $ Tp.Type 1
sortRule Ast.Type = return $ Tp.Sort $ Tp.Type 2

varRule :: Env -> Ast.Var -> Infer Tp.Term
varRule (Env env) v = case Map.lookup v env of
    Nothing -> throwError "Variable not in scope."
    Just t -> return t

absRule :: Env -> Ast.Binding -> Ast.Expr -> Ast.Location -> Infer Tp.Term
absRule env (Ast.Bind (v, e')) body loc =
    let v' = Ast.V v
     in do
            t <- getType env e'
            _ <- inferSort env e' -- mamy już e' przetłumaczone do termu
            bd <- infer (extend env (v', t)) body
            return $ Tp.ForAll v' t bd

piRule :: Env -> Ast.Binding -> Ast.Expr -> Ast.Location -> Infer Tp.Term
piRule env (Ast.Bind (v, tp)) body loc = do
    tp' <- getType env tp
    as <- inferSort env tp
    s <- inferSort (extend env (Ast.V v, tp')) body
    case s of
        Tp.Prop -> return $ Tp.Sort Tp.Prop
        Tp.Type n -> case as of
            Tp.Prop -> return $ Tp.Sort s
            Tp.Type i -> return $ Tp.Sort (Tp.Type (max n i))

appRule :: Env -> Ast.Expr -> Ast.Expr -> Infer Tp.Term
appRule env l r = do
    pi <- inferPi env l
    l' <- getType env l
    case pi of
        Tp.ForAll v i o -> do
            _ <- check env r i
            return $ Tp.subst v l' o
        _ -> throwError "Application to non-lambda"

inferSort :: Env -> Ast.Expr -> Infer Tp.Sort
inferSort env e = do
    t <- infer env e
    case Tp.normalize t of
        Tp.Sort s -> return s
        _ -> throwError "Expected sort"

inferPi :: Env -> Ast.Expr -> Infer Tp.Term
inferPi env e = do
    t <- infer env e
    case Tp.normalize t of
        pi@Tp.ForAll{} -> return pi
        _ -> throwError "Expected ForAll"

check :: Env -> Ast.Expr -> Tp.Term -> Infer ()
check env e tp = do
    tp' <- infer env e
    if Tp.normalize tp == Tp.normalize tp'
        then return ()
        else throwError "Type mismatch"

infer :: Env -> Ast.Expr -> Infer Tp.Term
infer env@(Env m) (e, loc) =
    case e of
        (Ast.Sort s) -> sortRule s
        (Ast.Var v) -> varRule env v
        (Ast.Lambda b e) -> absRule env b e loc
        (Ast.ForAll b e) -> piRule env b e loc
        (Ast.App l r) -> appRule env l r
        (Ast.LetIn b e body) -> undefined
  where
    keepEnv t = return (m, t)

getType :: Env -> Ast.Expr -> Infer Tp.Term
getType env (e, loc) = case e of
    (Ast.Sort Ast.Type) -> return (Tp.Sort $ Tp.Type 1)
    (Ast.Sort Ast.Prop) -> return (Tp.Sort Tp.Prop)
    Ast.Var v -> return $ Tp.Var v
    Ast.Lambda (Ast.Bind (v, e)) body -> do
        e' <- getType env e
        b' <- getType env body
        return $ Tp.Lambda (Ast.V v) e' b'
    Ast.ForAll (Ast.Bind (v, e)) body -> do
        e' <- getType env e
        b' <- getType env body
        return $ Tp.ForAll (Ast.V v) e' b'
    Ast.App l r -> do
        l' <- getType env l
        r' <- getType env r
        return $ Tp.App l' r'
    Ast.LetIn bind x0 x1 -> undefined