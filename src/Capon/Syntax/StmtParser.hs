module Capon.Syntax.StmtParser (
  parseStatement,
  parseStatements,
) where

import Data.Bifunctor
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Capon.Syntax.Ast
import Capon.Syntax.Lexer
import Capon.Syntax.Parser
import Capon.Syntax.Stmt

pInit :: Parser Statement
pInit = InitProof <$> (rword "proof" >> pExpr)

pIntro :: Parser Statement
pIntro = Intro <$> (rword "intro" >> pIdentifier)

pApply :: Parser Statement
pApply = apply <$> (rword "apply" >> pBody)
 where
  pBody = do
    e <- pExpr
    defs <- optional pWith
    pure (e, fromMaybe [] defs)
  apply (e, defs) = Apply e defs
  pWith = rword "with" *> sepBy1 pBinding (symbol ",")
  pBinding = do
    id <- pIdentifier
    symbol ":"
    e <- pExpr
    pure (id, e)

pQed :: Parser Statement
pQed = Qed <$ rword "qed"

pStmt :: Parser Statement
pStmt = choice [pInit, pIntro, pApply, pQed]

parseStatement :: String -> Text -> Either ParsingError Statement
parseStatement = fileParser pStmt

parseStatements :: String -> Text -> Either ParsingError [Statement]
parseStatements = fileParser pStmts
 where
  pStmts = sepBy pStmt $ symbol ";"