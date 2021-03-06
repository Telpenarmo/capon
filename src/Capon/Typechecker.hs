{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

module Capon.Typechecker (
    TypingError (..),
    inferType,
    checkType,
) where

import Control.Monad.Except
import Data.Text

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
        case whnf tp of
            Sort s -> return (t, s)
            _ -> throwError $ ExpectedType e tp

    inferPi :: Checkable a => Env -> a -> Infer a (Term, FData)
    inferPi env e = do
        (t, tp) <- infer env e
        case whnf tp of
            ForAll pi -> return (t, pi)
            _ -> throwError $ ExpectedFunction e tp

inferType :: Checkable a => Env -> a -> Either (TypingError a) (Term, Term)
inferType env = runExcept . infer env

checkType :: Checkable a => Env -> a -> Term -> Either (TypingError a) Term
checkType env m = runExcept . check env m

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
        (Ast.App f arg) -> appRule env f arg
        (Ast.LetIn b def body) -> letInRule env b def body loc
        (Ast.Error e) -> undefined
      where
        varRule :: Env -> Text -> AstInfer (Term, Term)
        varRule env v = case lookupType v env of
            Nothing -> throwError $ UnknownVar v
            Just t -> return (Var (var v), t)

        sortRule :: Ast.Sort -> AstInfer (Sort, Sort)
        sortRule Ast.Prop = return (Prop, Type 1)
        sortRule (Ast.Type i) = return (Type i, Type $ i + 1)

        absRule :: Env -> Ast.Binding -> Ast.Expr -> Ast.Location -> AstInfer (LData, FData)
        absRule env (Ast.Bind (v, tp)) body loc = do
            (ttp, _) <- inferSort env tp
            let env' = insertAbstract v ttp env
            (tbody, tbodytp) <- infer env' body
            return (LD v ttp tbody, FD v ttp tbodytp)

        piRule :: Env -> Ast.Binding -> Ast.Expr -> Ast.Location -> AstInfer (FData, Sort)
        piRule env (Ast.Bind (v, tp)) body loc = do
            (ttp, stp) <- inferSort env tp
            let env' = insertAbstract v ttp env
            (tbody, sbody) <- inferSort env' body
            let ret' = ret ttp tbody
             in case sbody of
                    Prop -> ret' Prop
                    Type n -> case stp of
                        Prop -> ret' sbody
                        Type n' -> ret' $ Type $ max n n'
          where
            ret a b s = return (FD v a b, s)

        appRule :: Env -> Ast.Expr -> Ast.Expr -> AstInfer (Term, Term)
        appRule env f arg = do
            (tf, FD v i o) <- inferPi env f
            targ <- check env arg i
            return (App tf targ, substitute v targ o)

        letInRule :: Env -> Ast.Binding -> Ast.Expr -> Ast.Expr -> Ast.Location -> AstInfer (Term, Term)
        -- let v:tp = def in body === (\v:tp -> body) arg
        letInRule env (Ast.Bind (v, tp)) def body loc = do
            (tp', _) <- inferSort env tp
            tdef <- check env def tp'
            let env' = insertDefined v tdef tp' env
            (tbody, tpbody) <- infer env' body
            return (substitute v tdef tbody, substitute v tdef tpbody)

instance Checkable Term where
    infer env t = withDef t $ case t of
        Var v -> varRule env v
        Sort s -> Sort <$> sortRule s
        App l r -> appRule env l r
        Lambda ld -> ForAll <$> absRule env ld
        ForAll fd -> Sort <$> piRule env fd
      where
        varRule :: Env -> Var -> Infer Term Term
        varRule env v = case lookupType x env of
            Nothing -> throwError $ UnknownVar x
            Just t -> return t
          where
            x = name v

        sortRule :: Sort -> Infer Term Sort
        sortRule Prop = return $ Type 1
        sortRule (Type i) = return $ Type $ i + 1

        appRule :: Env -> Term -> Term -> Infer Term Term
        appRule env f arg = do
            (tf, FD v i o) <- inferPi env f
            targ <- check env arg i
            return $ substitute v targ o

        absRule :: Env -> LData -> Infer Term FData
        absRule env (LD v tp body) = do
            (ttp, _) <- inferSort env tp
            let env' = insertAbstract v ttp env
            (_, tbodytp) <- infer env' body
            return $ FD v ttp tbodytp

        piRule :: Env -> FData -> Infer Term Sort
        piRule env (FD v tp body) = do
            (ttp, stp) <- inferSort env tp
            let env' = insertAbstract v ttp env
            (_, sbody) <- inferSort env' body
            case sbody of
                Prop -> return Prop
                Type n -> case stp of
                    Prop -> return sbody
                    Type n' -> return $ Type $ max n n'

withDef :: Term -> Infer Term Term -> Infer Term (Term, Term)
withDef t m = m >>= (\r -> return (t, r))
