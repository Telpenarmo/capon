{-# LANGUAGE DataKinds #-}
module Handlers (
  IState,
  handleCommand,
  displayType,
) where

import Console
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, get, put)
import Data.Text (Text, pack)
import System.Console.ANSI (getTerminalSize)

import Capon.Engine (IState, evalStatement)
import Capon.Pretty (Pretty, pretty)
import Capon.Proof (Proof, ProofState (..), assumptions, consequence)
import Capon.Syntax.Ast (Expr)
import Capon.Syntax.Parser (parseExpr)
import Capon.Syntax.Stmt (Statement (Abandon))
import Capon.Syntax.StmtParser (parseStatements)
import Capon.Typechecker (inferType, Inferred (typ))
import Capon.Types (Env, Term, toList)
import Prettyprinter (Doc, PageWidth (..), hardline, hcat, pageWidth, vcat, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..))

handleCommand :: (MonadIO m, MonadState IState m) => Text -> m ()
handleCommand t = do
  st <- get
  parseStatements "statement" t |>? flip loop st
 where
  loop [] st = updateState st
  loop (stmt : stmts) st =
    case stmt of
      Abandon -> do
        liftIO $ putStr "You are to abandon an unfinished proof. Are you sure?"
        confirmed <- liftIO $ confirm True
        when confirmed cont
      _ -> cont
   where
    cont = evalStatement st stmt |>> loop stmts

updateState :: (MonadIO m, MonadState IState m) => IState -> m ()
updateState st@(env, Nothing) = put st
updateState st@(env, Just pf) = do
  put st
  liftIO $ displayProof pf

displayProof :: Proof 'Incomplete -> IO ()
displayProof pf = do
  times <- height
  putStrLn $ replicate times '\n'
  renderOut $ pProof pf
 where
  height = maybe 5 ((`div` 2) . fst) <$> getTerminalSize

displayType :: (MonadIO m, MonadState IState m) => Text -> m ()
displayType = parseExpr "test" >?> go
 where
  go e = do
    (env, _) <- get
    inferType env e |>> (liftIO . renderOut . pretty . typ)

pEnv :: Env -> Doc AnsiStyle
pEnv env = vcat $ [pretty v <+> withColor Yellow ":" <+> pretty t | (v, t) <- assumptions]
 where
  assumptions :: [(Text, Term)]
  assumptions = map (\(a, (tp, _)) -> (a, tp)) $ toList env

pProof :: Proof 'Incomplete -> Doc AnsiStyle
pProof pf =
      pEnv (assumptions pf)
        <> hardline
        <> pageWidth ((withColor Black . hcat . flip Prelude.replicate "=") . maxWidth)
        <> hardline
        <> pretty (consequence pf)
 where
  maxWidth Unbounded = 50
  maxWidth (AvailablePerLine width ribbon) = width