{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Capon.Pretty where

import Control.Monad.Except (runExcept)
import qualified Data.Map as Map
import Data.Text (Text, replicate, unpack)
import Data.Void (Void)
import Prettyprinter hiding (Pretty, pretty)
import qualified Prettyprinter as PP
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

import qualified Capon.Proof as P
import qualified Capon.Syntax.Ast as Ast
import Capon.Syntax.Lexer (ParsingError (PErr))
import qualified Capon.Syntax.Stmt as Stmt
import Capon.Typechecker (TypingError (..))
import Capon.Types (toList)
import qualified Capon.Types as T

class Pretty a where
    pretty :: a -> Doc ann
    default pretty :: Show a => a -> Doc ann
    pretty = viaShow
    prettyPrec :: Int -> a -> Doc ann
    prettyPrec = const pretty

instance Pretty Text where pretty = PP.pretty
instance Pretty Int where pretty = PP.pretty
instance Pretty String where pretty = PP.pretty

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

pBinding :: Pretty p => Text -> p -> Doc ann
pBinding x tp = parens $ pretty x PP.<> ":" <+> pretty tp

instance Pretty T.Var
instance Show Ast.Location where
    show (Ast.Location l c f) = unpack f ++ ":" ++ show l ++ ":" ++ show c

instance Pretty Ast.Location where prettyPrec = const pretty

instance Pretty Ast.Binding where
    pretty (Ast.Bind (n, tp)) = pBinding n tp

pAbstraction :: (Pretty p) => Doc ann -> Int -> Text -> p -> p -> Doc ann
pAbstraction symbol p x tp bd =
    sep [symbol <> pBinding x tp, nest 2 $ "→" <+> pretty bd]

instance Pretty Ast.ExprData where
    pretty = prettyPrec 0
    prettyPrec _ (Ast.Var t) = pretty t
    prettyPrec _ (Ast.Sort Ast.Prop) = "Prop"
    prettyPrec _ (Ast.Sort (Ast.Type i)) = "Type" <+> pretty i
    prettyPrec p (Ast.Lambda (Ast.Bind (x, tp)) bd) =
        parensIf (p > 0) $ pAbstraction "λ" p x tp bd
    prettyPrec p (Ast.ForAll (Ast.Bind ("", tp)) bd) =
        parensIf (p > 4) $ sep [prettyPrec 5 tp, "⇒" <+> prettyPrec 4 bd]
    prettyPrec p (Ast.ForAll (Ast.Bind (x, tp)) bd) =
        parensIf (p > 0) $ pAbstraction "∀" p x tp bd
    prettyPrec p (Ast.App l r) =
        parensIf (p > 5) $ sep [prettyPrec 5 l, nest 2 $ prettyPrec 6 r]
    prettyPrec p (Ast.LetIn b e1 e2) =
        parensIf (p > 0) $
            sep
                [ "let"
                    <+> sep
                        [pretty b <+> "=", nest 2 $ pretty e1]
                , nest 2 $ "in" <+> pretty e2
                ]
    prettyPrec _ (Ast.Error t) = "Error:" <+> pretty t

instance Pretty Ast.Expr where
    pretty = prettyPrec 0
    prettyPrec n (e, _) = prettyPrec n e

getBindings :: Ast.ExprData -> [Ast.Binding]
getBindings (Ast.Lambda b (body, _)) = b : getBindings body
getBindings _ = []

viewBody :: Ast.Expr -> Ast.Expr
viewBody (Ast.Lambda _ a, _) = viewBody a
viewBody x = x

viewApp :: Ast.Expr -> Ast.Expr -> (Ast.Expr, [Ast.Expr])
viewApp e1 e2 = go e1 [e2]
  where
    go (Ast.App a b, _) xs = go a (b : xs)
    go f xs = (f, xs)

instance Pretty T.Term where
    pretty = prettyPrec 0
    prettyPrec _ (T.Var v) = pretty v
    prettyPrec _ (T.Sort T.Prop) = "Prop"
    prettyPrec _ (T.Sort (T.Type i)) = "Type" <+> pretty i
    prettyPrec p (T.Lambda (T.LD x tp bd)) =
        parensIf (p > 0) $ pAbstraction "λ" p x tp bd
    prettyPrec p (T.ForAll (T.FD "" tp bd)) =
        parensIf (p > 4) $ sep [prettyPrec 5 tp, "⇒" <+> prettyPrec 4 bd]
    prettyPrec p (T.ForAll (T.FD x tp bd)) =
        parensIf (p > 0) $ pAbstraction "∀" p x tp bd
    prettyPrec p (T.App l r) =
        parensIf (p > 5) $ sep [prettyPrec 5 l, nest 2 $ prettyPrec 6 r]

instance Pretty (TypingError Ast.Expr) where
    pretty (UnknownVar v) = "Variable not in scope:" <+> pretty v
    pretty (TypeMismatch (e, loc) actual expected) =
        fillSep
            [ pretty loc PP.<> ":"
            , "Expression "
            , cite e
            , "has type"
            , cite actual
            , "but"
            , cite expected
            , "was expected."
            ]
    pretty (ExpectedFunction (actual, loc) tp) =
        fillSep
            [ pretty loc PP.<> ":"
            , "Expression"
            , cite actual
            , "is not a function and cannot be applied."
            ]
    pretty (ExpectedType (actual, loc) tp) =
        fillSep
            [ pretty loc PP.<> ":"
            , "Expression"
            , cite actual
            , "has type"
            , pretty tp
            , "but a sort was expected."
            ]

instance Pretty (TypingError T.Term) where
    pretty (UnknownVar v) = "Variable not in scope:" <+> pretty v
    pretty (TypeMismatch e actual expected) =
        fillSep
            [ "Expression "
            , cite e
            , "has type"
            , cite actual
            , "but"
            , cite expected
            , "was expected."
            ]
    pretty (ExpectedFunction actual tp) =
        fillSep
            [ "Expression"
            , cite actual
            , "is not a function and cannot be applied."
            ]
    pretty (ExpectedType actual tp) =
        fillSep
            [ "Expression"
            , cite actual
            , "has type"
            , pretty tp
            , "but a sort was expected."
            ]

cite :: Pretty a => a -> Doc ann
cite = dquotes . pretty

instance Pretty P.ProovingError where
    pretty = \case
        P.NoMoreGoals -> "Proof is not yet complete."
        P.GoalLeft -> "Proof is already complete."
        P.ExpectedProduct -> "No product even after reduction."
        P.NotUnifiable from to ->
            fillSep ["Unable to unify", pretty from, "to", pretty to] <> "."
        P.WrongProof err -> fillSep ["The proof is wrong:", pretty err]
        P.ExpectedProp err ->
            fillSep
                [ "Typechecker failed with following error:"
                , pretty err
                ]
        P.TermError err env ->
            fillSep
                [ "Typechecker failed with following error:"
                , pretty err
                ]

instance Pretty Stmt.EngineError where
    pretty = \case
        Stmt.NoProof -> "No active proof."
        Stmt.ActiveProof -> "You have an unfinished proof."
        Stmt.ProovingErr err -> pretty err
        Stmt.TypingError err -> pretty err

ppEnv :: T.Env -> Doc ann
ppEnv env = vcat $ [pretty v <+> ":" <+> pretty t | (v, t) <- assumptions]
  where
    assumptions :: [(Text, T.Term)]
    assumptions = map (\(a, (tp, _)) -> (a, tp)) $ toList env

instance Pretty P.Proof where
    pretty pf =
        if P.completed pf
            then "No more subgoals."
            else
                ppEnv (P.assumptions pf)
                    <> hardline
                    <> pageWidth ((hcat . flip Prelude.replicate "=") . maxWidth)
                    <> hardline
                    <> pretty (P.consequence pf)
      where
        maxWidth Unbounded = 50
        maxWidth (AvailablePerLine width ribbon) = width

instance Pretty ParsingError where pretty (PErr e) = pretty $ errorBundlePretty e