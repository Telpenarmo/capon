{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- {-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Typechecker (typecheck, TypingError (..), Infer, checkAgainst) where

import qualified Ast
import Control.Monad.Except
import qualified Control.Monad.Except as Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import Data.Text
import Types

-- pattern A a b -> ()

data TypingError a
    = UnknownVar Text
    | TypeMismatch a Term Term
    | ExpectedFunction a Term
    | ExpectedType a Term

type Infer i o = Checkable i => Except (TypingError i) o
class Checkable a where
    -- type Infer t = Except InferenceError t
    infer :: Checkable a => Env -> a -> Infer a (Term, Term)
    check :: Checkable a => Env -> a -> Term -> Infer a Term
    check env@(Env map) e tp = do
        (et, etp) <- infer env e
        if normalize tp == normalize etp
            then return et
            else throwError $ TypeMismatch e etp tp
      where
        substAll t = Map.foldrWithKey substitute t map
    inferSort :: Checkable a => Env -> a -> Infer a (Term, Sort)
    inferSort env e = do
        (t, tp) <- infer env e
        case normalize tp of
            Sort s -> return (t, s)
            _ -> throwError $ ExpectedType e tp

    inferPi :: Checkable a => Env -> a -> Infer a (Term, FData)
    inferPi env e = do
        (t, tp) <- infer env e
        case normalize tp of
            ForAll pi -> return (t, pi)
            _ -> throwError $ ExpectedFunction e tp

typecheck :: Checkable a => a -> Either (TypingError a) (Term, Term)
typecheck = runExcept . infer emptyEnv

checkAgainst :: Checkable a => a -> Term -> Either (TypingError a) Term
checkAgainst m = runExcept . check emptyEnv m

type AstInfer a = Infer Ast.Expr a

instance Checkable Ast.Expr where
    infer env@(Env m) (e, loc) = case e of
        (Ast.Var v) -> varRule env v
        (Ast.Sort s) -> do
            (s, tp) <- sortRule s
            return (Sort s, Sort tp)
        (Ast.Lambda b e) -> do
            (ld, fd) <- absRule env b e loc
            return (Lambda ld, ForAll fd)
        (Ast.ForAll b e) -> do
            (fd, s) <- piRule env b e loc
            return (ForAll fd, Sort s)
        (Ast.App l r) -> appRule env l r
        (Ast.LetIn b e body) -> letInRule env b e body loc
        (Ast.Error e) -> undefined 
      where
        varRule :: Env -> Text -> AstInfer (Term, Term)
        varRule (Env env) v = case Map.lookup v env of
            Nothing -> throwError $ UnknownVar v
            Just t -> return (Var (V v 0), t)

        sortRule :: Ast.Sort -> AstInfer (Sort, Sort)
        sortRule Ast.Prop = return (Prop, Type 1)
        sortRule (Ast.Type i) = return (Type i, Type $ i + 1)

        absRule :: Env -> Ast.Binding -> Ast.Expr -> Ast.Location -> AstInfer (LData, FData)
        absRule env (Ast.Bind (v, arg)) body loc = do
            (at, _) <- inferSort env arg
            (bt, bd) <- infer (extend env (v, at)) body
            return (LD (v, at, bt), FD (v, at, bd))

        piRule :: Env -> Ast.Binding -> Ast.Expr -> Ast.Location -> AstInfer (FData, Sort)
        piRule env (Ast.Bind (v, arg)) body loc = do
            (at, as) <- inferSort env arg
            (bt, s) <- inferSort (extend env (v, at)) body
            let ret' = ret at bt
             in case s of
                    Prop -> ret' Prop
                    Type n -> case as of
                        Prop -> ret' s
                        Type n' -> ret' $ Type $ max n n'
          where
            ret a b s = return (FD (v, a, b), s)

        appRule :: Env -> Ast.Expr -> Ast.Expr -> AstInfer (Term, Term)
        appRule env l r = do
            (lt, FD (v, i, o)) <- inferPi env l
            rt <- check env r i
            return (App lt rt, substitute v lt o)

        letInRule :: Env -> Ast.Binding -> Ast.Expr -> Ast.Expr -> Ast.Location -> AstInfer (Term, Term)
        -- let v:tp = arg in body === (\v:tp -> body) arg
        letInRule env (Ast.Bind (v, tp)) arg body loc = do
            (tp', _) <- inferSort env tp -- typ argumentu
            (bt, btp) <- infer (extend env (v, tp')) body
            at <- check env arg tp' -- argument jako term
            return (substitute v at bt, substitute v at btp)

instance Checkable Term where
    infer env t = withDef t $ case t of
        Var v -> varRule env v
        Sort s -> Sort <$> sortRule s
        App l r -> appRule env l r
        Lambda ld -> ForAll <$> absRule env ld
        ForAll fd -> Sort <$> piRule env fd
      where
        varRule :: Env -> Var -> Infer Term Term
        varRule (Env env) (V v _) = case Map.lookup v env of
            Nothing -> throwError $ UnknownVar v
            Just t -> return t

        sortRule :: Sort -> Infer Term Sort
        sortRule Prop = return $ Type 1
        sortRule (Type i) = return $ Type $ i + 1

        appRule :: Env -> Term -> Term -> Infer Term Term
        appRule env l r = do
            (lt, FD (v, i, o)) <- inferPi env l
            rt <- check env r i
            return $ substitute v lt o

        absRule :: Env -> LData -> Infer Term FData
        absRule env (LD (v, tp, body)) = do
            (at, _) <- inferSort env tp
            (bt, bd) <- infer (extend env (v, at)) body
            return $ FD (v, at, bd)

        piRule :: Env -> FData -> Infer Term Sort
        piRule env (FD (v, tp, body)) = do
            (at, as) <- inferSort env tp
            (bt, s) <- inferSort (extend env (v, at)) body
            case s of
                Prop -> return Prop
                Type n -> case as of
                    Prop -> return s
                    Type n' -> return $ Type $ max n n'

withDef :: Term -> Infer Term Term -> Infer Term (Term, Term)
withDef t m = m >>= (\r -> return (t, r))
