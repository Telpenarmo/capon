module Handlers (
  IState,
  handleCommand,
  test,
  getProof,
) where

import Console
import Control.Monad.Except
import Control.Monad.State (MonadState (get, put))
import Data.Text (Text, pack)
import System.Console.ANSI (getTerminalSize)

import Capon.Engine
import Capon.Pretty (Pretty, renderP)
import Capon.Proof
import Capon.Syntax.Ast (Expr)
import Capon.Syntax.Parser (parseExpr)
import Capon.Syntax.Stmt
import Capon.Syntax.StmtParser
import Capon.Typechecker (typecheck)
import Capon.Types (Env, Term)

handleCommand :: (MonadIO m, MonadState IState m) => Text -> m ()
handleCommand t = do
  st <- get
  parseStatements "statement" t |>> (eval st >|> updateState)

getEnv :: MonadState IState m => m Env
getEnv = get >>= pure <$> fst

setEnv :: (MonadIO m, MonadState IState m) => Env -> m ()
setEnv env = do
  (_, pf) <- get
  put (env, pf)

getProof :: MonadState IState m => m (Maybe Proof)
getProof = get >>= pure <$> snd

putProof :: MonadState IState m => Proof -> m ()
putProof pf = do
  (env, _) <- get
  put (env, Just pf)

resetProof :: MonadState IState m => m ()
resetProof = do
  (env, _) <- get
  put (env, Nothing)

updateState :: (MonadIO m, MonadState IState m) => IState -> m ()
updateState st@(env, Nothing) = put st
updateState st@(env, Just pf) = do
  put st
  liftIO $ displayProof pf

displayProof :: Proof -> IO ()
displayProof pf = do
  times <- height
  putStrLn $ replicate times '\n'
  printP pf
 where
  height = maybe 5 ((`div` 2) . fst) <$> getTerminalSize

test :: Text -> IO ()
test = parseExpr "test" >|> typecheck >|> (liftIO . printP . snd)