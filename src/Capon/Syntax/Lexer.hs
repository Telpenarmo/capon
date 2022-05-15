module Capon.Syntax.Lexer where

import Data.Char (isAlphaNum)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser ()
symbol t = () <$ L.symbol sc t

recParseUntil :: Parser (Text -> a) -> Parser b -> Parser a -> Parser a
recParseUntil onErr end p = withRecovery recovery (p <* end)
 where
  recovery e = registerParseError e *> appEater
  appEater = onErr >>= (<$> eater)
  eater = pack <$> someTill (satisfy $ const True) end

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parensRec :: Parser (Text -> a) -> Parser a -> Parser a
parensRec onErr p = symbol "(" *> recParseUntil onErr (symbol ")") p

pIdentifier :: Parser Text
pIdentifier = (lexeme . try) (p >>= check)
 where
  p = (pack <$> some (satisfy isGood)) <?> "identifier"
  isGood c = isAlphaNum c || c == '_' || c == '\''
  check x =
    if x `elem` rws
      then fail $ "keyword " ++ show x ++ " cannot be an identifier"
      else return x
  rws = ["let", "in", "λ", "forall", "∀", "Type", "Prop", "with"]

rword :: Text -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

pInt :: Parser Int
pInt = L.decimal

newtype ParsingError = PErr (ParseErrorBundle Text Void)