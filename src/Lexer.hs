{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Data.Char (isAlphaNum)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser ()
symbol t = () <$ L.symbol sc t

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pIdentifier :: Parser Text
pIdentifier = (lexeme . try) (p >>= check)
  where
    p = (pack <$> some (satisfy isGood)) <?> "identifier"
    isGood c = isAlphaNum c || c == '_' || c == '\''
    check x =
        if x `elem` rws
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x
    rws = ["let", "in", "λ", "forall", "∀", "Type", "Prop"]

rword :: Text -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

pInt :: Parser Int
pInt = decimal

newtype ParsingError = PErr (ParseErrorBundle Text Void)