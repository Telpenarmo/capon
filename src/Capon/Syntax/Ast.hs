{-# LANGUAGE FlexibleInstances #-}

module Capon.Syntax.Ast (
    Var,
    Binding (..),
    Sort (..),
    ExprData (..),
    Expr (..),
    Location (..),
) where

import Data.Text (Text, unpack)

import Capon.Pretty (
    Pretty (..),
    nest,
    pAbstraction,
    pBinding,
    parensIf,
    sep,
    (<+>),
 )

type Var = Text
newtype Binding = Bind (Text, Expr)
data Sort = Type Int | Prop

type Expr = (ExprData, Location)

data ExprData
    = Sort Sort
    | Var Var
    | Lambda Binding Expr
    | ForAll Binding Expr
    | App Expr Expr
    | LetIn Binding Expr Expr
    | Error Text

data Location = Location {line :: Int, column :: Int, file :: Text}

instance Show Location where
    show (Location l c f) = unpack f ++ ":" ++ show l ++ ":" ++ show c

instance Pretty Location

instance Pretty Binding where
    pretty (Bind (n, tp)) = pBinding n tp

instance Pretty ExprData where
    pretty = prettyPrec 0
    prettyPrec _ (Var t) = pretty t
    prettyPrec _ (Sort Prop) = "Prop"
    prettyPrec _ (Sort (Type i)) = "Type" <+> pretty i
    prettyPrec p (Lambda (Bind (x, tp)) bd) =
        parensIf (p > 0) $ pAbstraction "λ" p x tp bd
    prettyPrec p (ForAll (Bind ("", tp)) bd) =
        parensIf (p > 4) $ sep [prettyPrec 5 tp, "⇒" <+> prettyPrec 4 bd]
    prettyPrec p (ForAll (Bind (x, tp)) bd) =
        parensIf (p > 0) $ pAbstraction "∀" p x tp bd
    prettyPrec p (App l r) =
        parensIf (p > 5) $ sep [prettyPrec 5 l, nest 2 $ prettyPrec 6 r]
    prettyPrec p (LetIn b e1 e2) =
        parensIf (p > 0) $
            sep
                [ "let"
                    <+> sep
                        [pretty b <+> "=", nest 2 $ pretty e1]
                , nest 2 $ "in" <+> pretty e2
                ]
    prettyPrec _ (Error t) = "Error:" <+> pretty t

instance Pretty Expr where
    pretty = prettyPrec 0
    prettyPrec n (e, _) = prettyPrec n e