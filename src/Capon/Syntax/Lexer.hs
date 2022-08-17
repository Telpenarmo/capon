{-# LANGUAGE MultiParamTypeClasses #-}

module Capon.Syntax.Lexer where

import Data.Bifunctor (first)
import Data.Char (isAlphaNum)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Error.Diagnose (Diagnostic, addFile, prettyDiagnostic)
import Error.Diagnose.Compat.Megaparsec (HasHints, errorDiagnosticFromBundle, hints)
import qualified Prettyprinter as PP
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Capon.Pretty (Error (errorDiagnostic), Pretty (pretty), unAnnotate)

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

newtype ParsingError = PErr (Diagnostic Text)

instance PP.Pretty ParsingError where pretty (PErr e) = unAnnotate $ prettyDiagnostic True 4 e
instance Pretty ParsingError where pretty (PErr e) = unAnnotate $ prettyDiagnostic True 4 e
instance Error ParsingError where errorDiagnostic (PErr e) = e
instance HasHints Void Text where hints = mempty

fileParser :: Parser a -> String -> Text -> Either ParsingError a
fileParser parser fileName content =
  first makeError $ runParser (sc *> parser <* eof) fileName content
 where
  makeError bundle = PErr err
   where
    err = addFile diag fileName (unpack content)
    diag = errorDiagnosticFromBundle Nothing "Parsing error" Nothing bundle
