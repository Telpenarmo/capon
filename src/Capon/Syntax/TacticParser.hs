module Capon.Syntax.TacticParser (
  parseProof,
  parseTactic,
  Tactic (..),
  Command (..),
) where

import Data.Bifunctor
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Capon.Syntax.Ast
import Capon.Syntax.Lexer
import Capon.Syntax.Parser

newtype Command = InitProof Expr
data Tactic = Intro Text | Apply Text | Rewrite Text | Qed

pIdPDot :: Text -> Parser a -> Parser a
pIdPDot id p = do
  _ <- rword id
  res <- p
  _ <- symbol "."
  return res

pProof :: Parser Command
pProof = InitProof <$> pIdPDot "proof" pExpr

pIntro :: Parser Tactic
pIntro = Intro <$> pIdPDot "intro" pIdentifier

pApply :: Parser Tactic
pApply = Apply <$> pIdPDot "apply" pIdentifier

pQed :: Parser Tactic
pQed = Qed <$ pIdPDot "qed" (rword "")

pTactic :: Parser Tactic
pTactic = choice [pIntro, pApply, pQed]

parseProof :: String -> Text -> Either ParsingError Command
parseProof s = first PErr . runParser (sc *> pProof) s

parseTactic :: String -> Text -> Either ParsingError Tactic
parseTactic s = first PErr . runParser (sc *> pTactic) s