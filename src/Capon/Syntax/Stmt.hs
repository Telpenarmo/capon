module Capon.Syntax.Stmt where

import Data.Text (Text)

import Capon.Pretty (Pretty (pretty))
import Capon.Proof (ProovingError)
import Capon.Syntax.Ast (Expr)
import Capon.Typechecker (TypingError)

type Definitions = [(Text, Expr)]

data Statement
    = InitProof Expr
    | Intro Text
    | Apply Expr Definitions
    | Rewrite Text Definitions
    | Unfold Text
    | Show Text
    | Define Text Expr
    | Abandon
    | Qed

data EngineError
    = NoProof
    | ActiveProof
    | ProovingErr ProovingError
    | TypingError (TypingError Expr)

instance Pretty EngineError where
    pretty = \case
        NoProof -> "No active proof."
        ActiveProof -> "You have an unfinished proof."
        ProovingErr err -> pretty err
        TypingError err -> pretty err