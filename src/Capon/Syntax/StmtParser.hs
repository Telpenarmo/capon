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
    id <- pIdentifier
    defs <- optional pWith
    pure (id, fromMaybe [] defs)
  apply (v, defs) = Apply v defs
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
parseStatement s = first PErr . runParser (sc *> pStmt <* eof) s

parseStatements :: String -> Text -> Either ParsingError [Statement]
parseStatements s = first PErr . runParser (sc *> pStmts <* eof) s
 where
  pStmts = sepBy pStmt $ symbol ";"