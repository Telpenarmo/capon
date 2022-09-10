module Capon.Syntax.Parser (parseExpr, pExpr) where

import Control.Monad.Combinators.Expr
import Data.Bifunctor (Bifunctor (first))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import Capon.Syntax.Ast
import Capon.Syntax.Lexer
import Data.Foldable
import Data.Set (singleton)

type EParser = Parser ExprData

withLoc :: Parser a -> Parser (a, Location)
withLoc parser = do
  pos <- getSourcePos
  x <- parser
  pure (x, toLoc pos)

pAnnotation :: Parser Expr
pAnnotation = (pColon *> pExpr) <?> "type annotation"
 where
  pColon = symbol ":"

pBinding :: Parser (Binding, Location)
pBinding = withLoc $ do
  v <- pIdentifier
  t <- pAnnotation
  pure $ Bind (v, t)

pAbstractor :: (Binding -> Expr -> ExprData) -> Parser () -> Parser Expr
pAbstractor makeAbstraction parseSymbol = do
  parseSymbol
  bindings <- recParseUntil eater arr parseBindings
  e <- pExpr
  pure $ foldr instantiate e bindings
 where
  instantiate (b, loc) e = (makeAbstraction b e, loc)
  parseBindings = (annotatedIds <|> groups) <?> "variable binding"
  groups = concat <$> some group
  group = parensRec eater annotatedIds
  annotatedIds = do
    ids <- some (withLoc pIdentifier)
    t <- pAnnotation
    pure $ map (\(id, loc) -> (Bind (id, t), loc)) ids

  eater :: Parser (Text -> [(Binding, Location)])
  eater = do
    eat <- bindEater
    pure $ \t -> [eat t]
  arr = (symbol "->" <|> symbol "→") <?> "arrow"

pLambda :: EParser
pLambda = do
  (e, loc) <- pAbstractor Lambda pSym
  pure e
 where
  pSym = (symbol "\\" <|> symbol "λ") <?> "lambda"

pForall :: EParser
pForall = do
  (e, loc) <- pAbstractor ForAll pSym
  pure e
 where
  pSym = (symbol "∀" <|> rword "forall") <?> "forall"

pLetIn :: EParser
pLetIn = do
  pLet
  (b, _) <- recParseUntil bindEater (symbol "=") pBinding
  e1 <- recParseUntil exprEater pIn pExpr
  LetIn b e1 <$> pExpr
 where
  pIn = rword "in"
  pLet = rword "let"
  pLetBinding = pBinding <|> parensRec bindEater pBinding

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

exprEater :: Parser (Text -> Expr)
exprEater = do
  pos <- getSourcePos
  pure $ \t -> (Error t, toLoc pos)

bindEater :: Parser (Text -> (Binding, Location))
bindEater = do
  pos <- getSourcePos
  eat <- exprEater
  pure $ \t -> (Bind ("#error", eat t), toLoc pos)

pExpr :: Parser Expr
pExpr = makeExprParser (nonApp <|> inParens) table <?> "expression"
 where
  inParens = parensRec exprEater pExpr
  nonApp = withLoc exp
  exp = choice [pVar, pLambda, pForall, pLetIn, pSort] <?> "expression"
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
toLoc (SourcePos f l c) = Location{line = unPos l, column = unPos c, Capon.Syntax.Ast.file = pack f}

parseExpr :: String -> Text -> Either ParsingError Expr
parseExpr = fileParser pExpr
