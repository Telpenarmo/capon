{-# LANGUAGE OverloadedStrings #-}

module Parser (parseExpr, pExpr) where

import Ast
import Lexer

import Control.Monad.Combinators.Expr
import Data.Bifunctor (Bifunctor (first))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Pretty
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

type EParser = Parser ExprData

pBinding :: Parser Binding
pBinding = (binder <|> parensRec recv binder) <?> "variable binding"
  where
    binder = do
        v <- pIdentifier
        t <- pAnn
        pure $ Bind (v, t)
    pAnn = (pColon *> pExpr) <?> "type annotation"
    pColon = symbol ":"
    recv = do
        err <- eatError
        return $ Bind ("v", err)

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
    b <- recParseUntil recv (symbol "=") pBinding
    e1 <- pExpr
    pIn
    LetIn b e1 <$> pExpr
  where
    recv = do
        err <- eatError
        return $ Bind ("v", err)
    pIn = rword "in"
    pLet = rword "let"

pVar :: EParser
pVar = (Var <$> pIdentifier) <?> "variable"

pSort :: EParser
pSort = Sort <$> (pType <|> pProp) <?> "sort"
  where
    pType = do
        rword "Type"
        i <- optional pInt
        return $ Type $ Data.Maybe.fromMaybe 1 i
    pProp = Prop <$ rword "Prop"

eatError :: Parser Expr
eatError = do
    pos <- getSourcePos
    dt <- packErr <$> eat
    pure (dt, toLoc pos)
  where
    eat :: Parser String
    eat = some ((eof >> (fail "eof" :: Parser Char)) <|> satisfy (/= ')'))
    packErr = Error . pack

pExpr :: Parser Expr
pExpr = makeExprParser (nonApp <|> inParens) table <?> "expression"
  where
    inParens = parensRec eatError pExpr
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
                    pure (\l r -> (ForAll (Bind ("", l)) r, toLoc pos))
                )
            ]
        ]
toLoc :: SourcePos -> Location
toLoc (SourcePos f l c) = Location{line = unPos l, column = unPos c, file = pack f}

parseExpr :: String -> Text -> Either ParsingError Expr
parseExpr s = first PErr . runParser (sc *> pExpr) s