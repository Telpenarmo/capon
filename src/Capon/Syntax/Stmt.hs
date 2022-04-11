module Capon.Syntax.Stmt where

import Data.Text (Text)

import Capon.Proof (ProovingError)
import Capon.Syntax.Ast (Expr)

type Definitions = [(Text, Expr)]

data Statement
    = InitProof Expr
    | Intro Text
    | Apply Text Definitions
    | Rewrite Text Definitions
    | Unfold Text
    | Show Text
    | Define Text Expr
    | Abandon
    | Qed

data EngineError = NoProof | ActiveProof | ProovingErr ProovingError