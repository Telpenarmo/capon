{-# LANGUAGE OverloadedStrings #-}

module Parser (parseExpr, testParser) where

import Ast

import Control.Monad.Combinators.Expr
import Data.Char (isAlphaNum)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text
type EParser = Parser ExprData

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

pBinding :: Parser Binding
pBinding = (binder <|> parens binder) <?> "variable binding"
  where
    binder = do
        v <- pIdentifier
        t <- pAnn
        pure $ Bind (v, t)
    pAnn = (pColon *> pExpr) <?> "type annotation"
    pColon = symbol ":"

pAbstractor :: (Binding -> Expr -> ExprData) -> Parser () -> EParser
pAbstractor f sym = do
    sym
    b <- pBinding
    (symbol "->" <|> symbol "→") <?> "arrow"
    f b <$> pExpr

pLambda :: EParser
pLambda = pAbstractor Lambda pSym
  where
    pSym = (symbol "\\" <|> symbol "λ") <?> "lambda"

pForall :: EParser
pForall = pAbstractor ForAll pSym
  where
    pSym = (symbol "∀" <|> rword "forall") <?> "forall"

pLetIn :: EParser
pLetIn = do
    pLet
    b <- pBinding
    symbol "="
    e1 <- pExpr
    pIn
    LetIn b e1 <$> pExpr
  where
    pIn = rword "in"
    pLet = rword "let"

pVar :: EParser
pVar = (Var . V <$> pIdentifier) <?> "variable"

pSort :: EParser
pSort = Sort <$> (pType <|> pProp) <?> "sort"
  where
    pType = Type <$ rword "Type"
    pProp = Prop <$ rword "Set"

pExpr :: Parser Expr
pExpr = makeExprParser (nonApp <|> parens pExpr) table <?> "expression"
  where
    nonApp = do
        pos <- getSourcePos
        dt <- choice [pLambda, pForall, pVar, pLetIn, pSort]
        pure (dt, toLoc pos)
    table =
        [
            [ InfixL -- application (A B)
                ( do
                    pos <- getSourcePos

                    pure (\l r -> (App l r, toLoc pos))
                )
            ]
        ,
            [ InfixR -- arrow type (A ⇒ B)
                ( do
                    (symbol "=>" <|> symbol "⇒") <?> "double arrow"
                    pos <- getSourcePos
                    pure (\l r -> (ForAll (Bind ("#_", l)) r, toLoc pos))
                )
            ]
        ]
    toLoc (SourcePos f l c) = Location{line = unPos l, column = unPos c, file = pack f}

parseExpr :: String -> Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = runParser (sc *> pExpr)

testParser :: Text -> IO ()
testParser = parseTest (sc *> pExpr)