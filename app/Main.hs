module Main (main) where

import qualified Ast
import Control.Monad.Except
import Control.Monad.State
import Data.Text (Text, pack)
import Handlers
import System.Console.Repline
import System.Environment (getArgs)

type Repl a = HaskelineT (StateT IState IO) a

prompt :: MultiLine -> Repl String
prompt = \case
  MultiLine -> pure ":"
  SingleLine -> get >>= pure <$> maybe ">>> " (const "prooving > ")

cmd :: String -> Repl ()
cmd s = do
  st <- get
  maybe handleNewProof handleTactic st $ pack s

help :: String -> IO ()
help args = putStrLn $ "Help: " ++ args

opts :: [(String, String -> Repl ())]
opts =
  [ ("help", liftIO . help)
  , ("test", liftIO . test . pack)
  ]

completer :: (Monad m, MonadState IState m) => WordCompleter m
completer n = return ["foo"]

final :: Repl ExitDecision
final = Exit <$ liftIO (putStrLn "Goodbye!")

main :: IO ()
main =
  flip evalStateT Nothing $ evalReplOpts ropts
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