{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Capon.Typechecker (
    TypingError (..),
    Inferred (..),
    inferType,
    checkType,
) where

import Control.Monad.Except
import Data.Text

import Capon.Pretty (Doc, Pretty (pretty), cite, fillSep, (<+>))
import qualified Capon.Syntax.Ast as Ast
import Capon.Types
import Data.Functor ((<&>))

data TypingError a
    = UnknownVar Text
    | TypeMismatch a Term Term
    | ExpectedFunction a Term
    | ExpectedType a Term

prettyError :: (Pretty a) => TypingError a -> Doc ann
prettyError (UnknownVar v) = "Variable not in scope:" <+> pretty v
prettyError (TypeMismatch e actual expected) =
    fillSep
        [ "Expression"
        , cite e
        , "has type"
        , cite actual
        , "but"
        , cite expected
        , "was expected."
        ]
prettyError (ExpectedFunction actual tp) =
    fillSep
        [ "Expression"
        , cite actual
        , "is not a function and cannot be applied."
        ]
prettyError (ExpectedType actual tp) =
    fillSep
        [ "Expression"
        , cite actual
        , "has type"
        , pretty tp
        , "but a sort was expected."
        ]

instance Pretty a => Pretty (TypingError a) where pretty = prettyError

type Infer i o = Checkable i => Except (TypingError i) o

data Inferred a b = Inferred
    { term :: a
    , typ :: b
    }

class Checkable a where
    -- type Infer t = Except InferenceError t
    infer :: Checkable a => Env -> a -> Infer a (Inferred Term Term)
    check :: Checkable a => Env -> a -> Term -> Infer a Term
    check env e tp = do
        Inferred{..} <- infer env e
        if eval env tp == eval env typ
            then return term
            else throwError $ TypeMismatch e typ tp

    inferSort :: Checkable a => Env -> a -> Infer a (Inferred Term Sort)
    inferSort env e = do
        Inferred{..} <- infer env e
        case whnf typ of
            Sort s -> return Inferred{term = term, typ = s}
            _other -> throwError $ ExpectedType e typ

    inferPi :: Checkable a => Env -> a -> Infer a (Inferred Term FData)
    inferPi env e = do
        Inferred{..} <- infer env e
        case whnf typ of
            ForAll pi -> return Inferred{term = term, typ = pi}
            _ -> throwError $ ExpectedFunction e typ

inferType :: Checkable a => Env -> a -> Either (TypingError a) (Inferred Term Term)
inferType env = runExcept . infer env

checkType :: Checkable a => Env -> a -> Term -> Either (TypingError a) Term
checkType env m = runExcept . check env m

type AstInfer a = Infer Ast.Expr a

instance Checkable Ast.Expr where
    infer env (e, loc) = case e of
        (Ast.Var v) -> varRule env v
        (Ast.Sort s) -> do
            (s, tp) <- sortRule s
            return Inferred{term = Sort s, typ = Sort tp}
        (Ast.Lambda b e) -> do
            Inferred{..} <- absRule env b e loc
            return Inferred{term = Lambda term, typ = ForAll typ}
        (Ast.ForAll b e) -> do
            Inferred{..} <- piRule env b e loc
            return Inferred{term = ForAll term, typ = Sort typ}
        (Ast.App f arg) -> appRule env f arg
        (Ast.LetIn b def body) -> letInRule env b def body loc
        (Ast.Error e) -> undefined
      where
        varRule :: Env -> Text -> AstInfer (Inferred Term Term)
        varRule env v = case lookupType v env of
            Nothing -> throwError $ UnknownVar v
            Just typ -> return Inferred{term, typ} where term = Var $ var v

        sortRule :: Ast.Sort -> AstInfer (Sort, Sort)
        sortRule Ast.Prop = return (Prop, Type 1)
        sortRule (Ast.Type i) = return (Type i, Type $ i + 1)

        absRule :: Env -> Ast.Binding -> Ast.Expr -> Ast.Location -> AstInfer (Inferred LData FData)
        absRule env (Ast.Bind (v, tp)) body loc = do
            Inferred{term = ttp} <- inferSort env tp
            let env' = insertAbstract v ttp env
            Inferred{term = tbody, typ = tbodytp} <- infer env' body
            let term = LD v ttp tbody
            let typ = FD v ttp tbodytp
            return Inferred{term, typ}

        piRule :: Env -> Ast.Binding -> Ast.Expr -> Ast.Location -> AstInfer (Inferred FData Sort)
        piRule env (Ast.Bind (v, tp)) body loc = do
            Inferred ttp stp <- inferSort env tp
            let env' = insertAbstract v ttp env
            Inferred tbody sbody <- inferSort env' body
            let ret' = ret ttp tbody
             in case sbody of
                    Prop -> ret' Prop
                    Type n -> case stp of
                        Prop -> ret' sbody
                        Type n' -> ret' $ Type $ max n n'
          where
            ret a b typ = return Inferred{term = FD v a b, typ}

        appRule :: Env -> Ast.Expr -> Ast.Expr -> AstInfer (Inferred Term Term)
        appRule env f arg = do
            Inferred tf (FD v i o) <- inferPi env f
            targ <- check env arg i
            return Inferred{term = App tf targ, typ = substitute v targ o}

        letInRule :: Env -> Ast.Binding -> Ast.Expr -> Ast.Expr -> Ast.Location -> AstInfer (Inferred Term Term)
        -- let v:tp = def in body === (\v:tp -> body) arg
        letInRule env (Ast.Bind (v, tp)) def body loc = do
            Inferred tp' _ <- inferSort env tp
            tdef <- check env def tp'
            let env' = insertDefined v tdef tp' env
            Inferred tbody tpbody <- infer env' body
            return Inferred{term = substitute v tdef tbody, typ = substitute v tdef tpbody}

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
            Inferred tf (FD v i o) <- inferPi env f
            targ <- check env arg i
            return $ substitute v targ o

        absRule :: Env -> LData -> Infer Term FData
        absRule env (LD v tp body) = do
            Inferred ttp _ <- inferSort env tp
            let env' = insertAbstract v ttp env
            Inferred _ tbodytp <- infer env' body
            return $ FD v ttp tbodytp

        piRule :: Env -> FData -> Infer Term Sort
        piRule env (FD v tp body) = do
            Inferred ttp stp <- inferSort env tp
            let env' = insertAbstract v ttp env
            Inferred _ sbody <- inferSort env' body
            case sbody of
                Prop -> return Prop
                Type n -> case stp of
                    Prop -> return sbody
                    Type n' -> return $ Type $ max n n'

withDef :: Term -> Infer Term Term -> Infer Term (Inferred Term Term)
withDef term m = m <&> Inferred term
