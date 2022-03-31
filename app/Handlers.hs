module Handlers (
  IState,
  handleNewProof,
  handleTactic,
  test,
  getEnv,
  getProof,
  putProof,
  resetProof,
) where

import Console
import Control.Monad.Except
import Control.Monad.State (MonadState (get, put))
import Data.Text (Text, pack)
import System.Console.ANSI (getTerminalSize)

import qualified Capon.Context as Context
import Capon.Proof
import Capon.Syntax.Parser (parseExpr)
import Capon.Syntax.TacticParser
import Capon.Typechecker (typecheck)
import Capon.Types (Env, Term)

type IState = (Env, Maybe Proof)

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

updateProof :: (MonadIO m, MonadState IState m) => Proof -> m ()
updateProof pf = do
  putProof pf
  liftIO $ displayProof pf

evalTactic :: (MonadIO m, MonadState IState m) => Tactic -> Proof -> m ()
evalTactic = eval
 where
  eval (Intro n) = update . intro n
  eval (Apply n) = update . applyAssm n
  eval (Rewrite n) = error "not implemented"
  eval Qed = (runExcept >|> finish) . qed
  update = runExcept >|> updateProof
  finish t = do
    resetProof
    liftIO $ putStr "Proof: " >> printSuccess t

displayProof :: Proof -> IO ()
displayProof pf = do
  times <- height
  putStrLn $ replicate times '\n'
  printP pf
 where
  height = maybe 5 ((`div` 2) . fst) <$> getTerminalSize

handleTactic :: (MonadIO m, MonadState IState m) => Proof -> Text -> m ()
handleTactic pf = parseTactic "tactic" >|> flip evalTactic pf

handleNewProof :: (MonadIO m, MonadState IState m) => Text -> m ()
handleNewProof = parseProof "theorem" >|> check >|> (go . fst)
 where
  check (InitProof e) = typecheck e
  go t = getEnv >>= updateProof . flip proof t

test :: Text -> IO ()
test = parseExpr "test" >|> typecheck >|> (liftIO . printP . snd)