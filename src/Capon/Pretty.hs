{-# LANGUAGE FlexibleInstances #-}

module Capon.Pretty (renderP, renderPLen, Pretty) where

import Control.Monad.Except (runExcept)
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Text.PrettyPrint as PP

import qualified Capon.Context as Context
import qualified Capon.Proof as P
import qualified Capon.Syntax.Ast as Ast
import Capon.Syntax.Lexer (ParsingError (PErr))
import Capon.Typechecker (TypingError (..))
import qualified Capon.Types as T

class Pretty p where
    ppPrec :: Int -> p -> Doc

    -- ppPrec _ = pp
    pp :: p -> Doc
    pp = ppPrec 0

renderPLen :: Pretty a => Int -> a -> String
renderPLen lineLength a = renderStyle style $ pp a where style = Style PageMode lineLength 1
renderP :: Pretty a => a -> String
renderP = renderPLen 60

instance Pretty String where
    ppPrec _ s = text s

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

ppBinding :: Pretty p => Text -> p -> Doc
ppBinding n tp = parens $ text (unpack n) PP.<> ":" <+> ppPrec 0 tp

instance Show Ast.Location where
    show (Ast.Location l c f) = unpack f ++ ":" ++ show l ++ ":" ++ show c

instance Pretty Ast.Location where ppPrec _ = text . show

instance Pretty Ast.Binding where
    ppPrec p (Ast.Bind (n, tp)) = ppBinding n tp

instance Pretty Ast.ExprData where
    ppPrec _ (Ast.Var t) = text $ unpack t
    ppPrec _ (Ast.Sort Ast.Prop) = "Prop"
    ppPrec _ (Ast.Sort (Ast.Type i)) = "Type" <+> int i
    ppPrec p (Ast.Lambda b bd) = parensIf (p > 0) $ hang ("λ" PP.<> ppPrec 0 b <+> "→") 2 (ppPrec 0 bd)
    ppPrec p (Ast.ForAll (Ast.Bind ("", tp)) bd) = parensIf (p > 4) $ ppPrec 5 tp <+> "⇒" <+> ppPrec 4 bd
    ppPrec p (Ast.ForAll b bd) = parensIf (p > 0) $ hang ("∀" PP.<> ppPrec 0 b <+> "→") 2 (ppPrec 0 bd)
    ppPrec p (Ast.App l r) = parensIf (p > 5) $ ppPrec 5 l <+> ppPrec 6 r
    ppPrec p (Ast.LetIn (Ast.Bind (n, tp)) e1 e2) =
        parensIf (p > 0) $ hang ("let" <+> (pp . unpack $ n) <+> "=" <+> ppPrec 0 e1 <+> "in") 2 (ppPrec 0 e2)
    ppPrec _ (Ast.Error t) = "Error" <+> text (unpack t)

instance Pretty Ast.Expr where ppPrec n (e, _) = ppPrec n e

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

instance Pretty T.Var where
    ppPrec _ = text . show

instance Pretty T.Term where
    ppPrec _ (T.Var v) = pp v
    ppPrec _ (T.Sort T.Prop) = "Prop"
    ppPrec _ (T.Sort (T.Type i)) = "Type" <+> int i
    ppPrec p (T.Lambda (T.LD v tp bd)) = parensIf (p > 0) $ "λ" PP.<> ppBinding v tp <+> "→" <+> ppPrec 0 bd
    ppPrec p (T.ForAll (T.FD "" tp bd)) = parensIf (p > 4) $ ppPrec 5 tp <+> "⇒" <+> ppPrec 4 bd
    ppPrec p (T.ForAll (T.FD v tp bd)) = parensIf (p > 0) $ "∀" PP.<> ppBinding v tp <+> "→" <+> ppPrec 0 bd
    ppPrec p (T.App l r) = parensIf (p > 5) $ ppPrec 5 l <+> ppPrec 6 r

instance Pretty (TypingError Ast.Expr) where
    ppPrec _ (UnknownVar v) = "Variable not in scope:" <+> text (unpack v)
    ppPrec _ (TypeMismatch (e, loc) actual expected) =
        pp loc PP.<> ":" <+> "Expression " <+> cite e <+> "has type" <+> cite actual <+> "but" <+> cite expected <+> "was expected."
    ppPrec _ (ExpectedFunction (actual, loc) tp) =
        pp loc PP.<> ":" <+> "Expression" <+> cite actual <+> "is not a function and cannot be applied."
    ppPrec _ (ExpectedType (actual, loc) tp) =
        pp loc PP.<> ":" <+> "Expression" <+> cite actual <+> "has type" <+> pp tp <+> "but a sort was expected."

instance Pretty (TypingError T.Term) where
    ppPrec _ (UnknownVar v) = "Variable not in scope:" <+> text (unpack v)
    ppPrec _ (TypeMismatch e actual expected) =
        "Expression " <+> cite e <+> "has type" <+> cite actual <+> "but" <+> cite expected <+> "was expected."
    ppPrec _ (ExpectedFunction actual tp) =
        "Expression" <+> cite actual <+> "is not a function and cannot be applied."
    ppPrec _ (ExpectedType actual tp) =
        "Expression" <+> cite actual <+> "has type" <+> pp tp <+> "but a sort was expected."

cite :: Pretty a => a -> Doc
cite = quotes . pp

instance Pretty P.ProovingError where
    ppPrec _ = \case
        P.NoMoreGoals -> "Proof is not yet complete."
        P.GoalLeft -> "Proof is already complete."
        P.ExpectedProduct -> "No product even after reduction."
        P.ExpectedPiOrGoal t -> "Invalid application of" <+> pp t
        P.AssumptionNotFound v -> "The reference" <+> text (unpack v) <+> "was not found in the current environment."
        P.WrongProof err -> "The proof is wrong:" <+> pp err
        P.ExpectedProp err -> "Typechecker failed with following error:" $$ pp err

ppEnv :: T.Env -> Doc
ppEnv env = vcat $ [text (unpack v) <+> ":" <+> pp t | (v, t) <- assumptions]
  where
    assumptions :: [(Text, T.Term)]
    assumptions = map (\(a, (tp, _)) -> (a, tp)) $ Context.toList env

instance Pretty P.Proof where
    ppPrec _ pf =
        if P.completed pf
            then "No more subgoals."
            else ppEnv (P.assumptions pf) $$ sizedText 50 (replicate 80 '=') $$ pp (P.consequence pf)

instance Pretty ParsingError where ppPrec _ (PErr e) = text $ errorBundlePretty e

-- instance Show P.Proof where show = renderP