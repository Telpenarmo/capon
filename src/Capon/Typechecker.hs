{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

module Capon.Typechecker (typecheck, TypingError (..), Infer, checkAgainst) where

import Control.Monad.Except
import Data.Text

import qualified Capon.Context as Context
import qualified Capon.Syntax.Ast as Ast
import Capon.Types

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
    check env e tp = do
        (et, etp) <- infer env e
        if eval env tp == eval env etp
            then return et
            else throwError $ TypeMismatch e etp tp

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
typecheck = runExcept . infer Context.empty

checkAgainst :: Checkable a => a -> Term -> Either (TypingError a) Term
checkAgainst m = runExcept . check Context.empty m

type AstInfer a = Infer Ast.Expr a

instance Checkable Ast.Expr where
    infer env (e, loc) = case e of
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
        varRule env v = case Context.lookupType v env of
            Nothing -> throwError $ UnknownVar v
            Just t -> return (Var (var v), t)

        sortRule :: Ast.Sort -> AstInfer (Sort, Sort)
        sortRule Ast.Prop = return (Prop, Type 1)
        sortRule (Ast.Type i) = return (Type i, Type $ i + 1)

        absRule :: Env -> Ast.Binding -> Ast.Expr -> Ast.Location -> AstInfer (LData, FData)
        absRule env (Ast.Bind (v, arg)) body loc = do
            (at, _) <- inferSort env arg
            (bt, bd) <- infer (Context.insertAbstract v at env) body
            return (LD v at bt, FD v at bd)

        piRule :: Env -> Ast.Binding -> Ast.Expr -> Ast.Location -> AstInfer (FData, Sort)
        piRule env (Ast.Bind (v, arg)) body loc = do
            (at, as) <- inferSort env arg
            (bt, s) <- inferSort (Context.insertAbstract v at env) body
            let ret' = ret at bt
             in case s of
                    Prop -> ret' Prop
                    Type n -> case as of
                        Prop -> ret' s
                        Type n' -> ret' $ Type $ max n n'
          where
            ret a b s = return (FD v a b, s)

        appRule :: Env -> Ast.Expr -> Ast.Expr -> AstInfer (Term, Term)
        appRule env l r = do
            (lt, FD v i o) <- inferPi env l
            rt <- check env r i
            return (App lt rt, substitute v lt o)

        letInRule :: Env -> Ast.Binding -> Ast.Expr -> Ast.Expr -> Ast.Location -> AstInfer (Term, Term)
        -- let v:tp = arg in body === (\v:tp -> body) arg
        letInRule env (Ast.Bind (v, tp)) arg body loc = do
            (tp', _) <- inferSort env tp -- typ argumentu
            at <- check env arg tp' -- argument jako term
            (bt, btp) <- infer (Context.insertDefined v at tp' env) body
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
        varRule env v = case Context.lookupType x env of
            Nothing -> throwError $ UnknownVar x
            Just t -> return t
          where
            x = name v

        sortRule :: Sort -> Infer Term Sort
        sortRule Prop = return $ Type 1
        sortRule (Type i) = return $ Type $ i + 1

        appRule :: Env -> Term -> Term -> Infer Term Term
        appRule env l r = do
            (lt, FD v i o) <- inferPi env l
            rt <- check env r i
            return $ substitute v lt o

        absRule :: Env -> LData -> Infer Term FData
        absRule env (LD v tp body) = do
            (at, _) <- inferSort env tp
            (bt, bd) <- infer (Context.insertAbstract v at env) body
            return $ FD v at bd

        piRule :: Env -> FData -> Infer Term Sort
        piRule env (FD v tp body) = do
            (at, as) <- inferSort env tp
            (bt, s) <- inferSort (Context.insertAbstract v at env) body
            case s of
                Prop -> return Prop
                Type n -> case as of
                    Prop -> return s
                    Type n' -> return $ Type $ max n n'

withDef :: Term -> Infer Term Term -> Infer Term (Term, Term)
withDef t m = m >>= (\r -> return (t, r))
