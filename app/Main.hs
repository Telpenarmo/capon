module Main (main) where

import Console
import Control.Monad.Except
import Control.Monad.State
import Data.Text (Text, pack)
import Handlers
import System.Console.Repline
import System.Environment (getArgs)
import System.Exit (exitSuccess)

import qualified Capon.Syntax.Ast as Ast
import Capon.Types (emptyEnv)

type Repl a = HaskelineT (StateT IState IO) a

prompt :: MultiLine -> Repl String
prompt = \case
  MultiLine -> pure ": "
  SingleLine -> do
    (_, pf) <- get
    pure $ maybe ">>> " (const "prooving > ") pf

cmd :: String -> Repl ()
cmd = handleCommand . pack

help :: String -> IO ()
help args =
  putStrLn $
    "Commands available from the prompt:\n"
      ++ "\n"
      ++ "  :help, :?         display this list\n"
      ++ "  :type <expr>      show the type of <expr>\n"
      ++ "  :quit             exit Capon\n"

opts :: [(String, String -> Repl ())]
opts =
  [ ("?", liftIO . help)
  , ("help", liftIO . help)
  , ("type", liftIO . test . pack)
  , ("quit", const quit)
  ]

quit :: Repl ()
quit = do
  decision <- final
  case decision of
    Continue -> return ()
    Exit -> liftIO exitSuccess

completer :: (Monad m, MonadState IState m) => WordCompleter m
completer n = return ["foo"]

final :: Repl ExitDecision
final = do
  (_, pf) <- get
  case pf of
    Nothing -> exit
    Just pr -> do
      liftIO $ putStr "You have an unfinished proof. Are you sure?"
      confirmed <- liftIO $ confirm True
      if confirmed then exit else return Continue
 where
  exit = Exit <$ liftIO (putStrLn "Goodbye!")

main :: IO ()
main =
  flip evalStateT (emptyEnv, Nothing) $ evalReplOpts ropts
 where
  ropts =
    ReplOpts
      { banner = prompt
      , command = cmd
      , options = opts
      , prefix = Just ':'
      , multilineCommand = Nothing
      , tabComplete = Word0 completer
      , initialiser = liftIO $ putStrLn "Welcome to Capon!"
      , finaliser = final
      }