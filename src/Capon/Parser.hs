module Capon.Parser (parseExpr, pExpr) where

import Control.Monad.Combinators.Expr
import Data.Bifunctor (Bifunctor (first))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import Capon.Ast
import Capon.Lexer

type EParser = Parser ExprData

pBinding :: Parser Binding
pBinding = (binder <|> parensRec eatBind binder) <?> "variable binding"
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
  b <- recParseUntil eatBind arr pBinding
  f b <$> pExpr
 where
  arr = (symbol "->" <|> symbol "→") <?> "arrow"

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
  b <- recParseUntil eatBind (symbol "=") pBinding
  e1 <- recParseUntil eatError pIn pExpr
  LetIn b e1 <$> pExpr
 where
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
    return $ Type $ fromMaybe 1 i
  pProp = Prop <$ rword "Prop"

eatError :: Parser (Text -> Expr)
eatError = do
  pos <- getSourcePos
  pure $ \t -> (Error t, toLoc pos)

eatBind :: Parser (Text -> Binding)
eatBind = do
  err <- eatError
  return $ \t -> Bind ("#error", err t)

pExpr :: Parser Expr
pExpr = makeExprParser (nonApp <|> inParens) table <?> "expression"
 where
  inParens = parensRec eatError pExpr
  nonApp = do
    pos <- getSourcePos
    dt <- choice [pVar, pLambda, pForall, pLetIn, pSort] <?> "expression"
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