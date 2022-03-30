module Handlers (IState, handleNewProof, handleTactic, test) where

import Console
import qualified Context
import Control.Monad.Except
import Control.Monad.State (MonadState (put))
import Data.Text (Text, pack)
import Parser (parseExpr)
import Proof
import System.Console.ANSI (getTerminalSize)
import TacticParser
import Typechecker (typecheck)

type IState = Maybe Proof

updateState :: (MonadIO m, MonadState IState m) => Proof -> m ()
updateState pf = do
  put $ Just pf
  liftIO $ displayProof pf

evalTactic :: (MonadIO m, MonadState IState m) => Tactic -> Proof -> m ()
evalTactic = eval
 where
  eval (Intro n) = updateProof . intro n
  eval (Apply n) = updateProof . applyAssm n
  eval (Rewrite n) = error "not implemented"
  eval Qed = (runExcept >|> finish) . qed
  updateProof = runExcept >|> updateState
  finish t = do
    put Nothing
    liftIO $ putStr "Proof: " >> printSuccess t

displayProof :: Proof -> IO ()
displayProof pf = do
  times <- height
  putStrLn $ replicate times '\n'
  printP pf
 where
  height = do
    i <- getTerminalSize
    case i of
      Nothing -> return 5
      Just (h, _) -> return $ h `div` 2

handleTactic :: (MonadIO m, MonadState IState m) => Proof -> Text -> m ()
handleTactic pf = parseTactic "tactic" >|> flip evalTactic pf

handleNewProof :: (MonadIO m, MonadState IState m) => Text -> m ()
handleNewProof = parseProof "theorem" >|> check >|> (updateState . proof Context.empty . fst)
 where
  check (InitProof e) = typecheck e

test :: Text -> IO ()
test = parseExpr "test" >|> typecheck >|> (liftIO . printP . snd)