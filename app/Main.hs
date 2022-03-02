{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import qualified Ast
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.List (isPrefixOf)
import Data.Text (Text, pack)
import Parser (parseExpr)
import Pretty
import Proof
import System.Console.ANSI
import System.Console.Repline
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (isEOF)
import TacticParser
import Typechecker
import Types

type IState = Maybe Proof
type Repl a = HaskelineT (StateT IState IO) a

(|>>) :: Pretty e => Either e a -> (a -> Repl ()) -> Repl ()
ex |>> cont = case ex of
  Left e -> liftIO $ printError e
  Right a -> cont a

infixr 1 >|>
(>|>) :: Pretty e => (t -> Either e a) -> (a -> Repl ()) -> t -> Repl ()
f >|> g = \x -> f x |>> g

prompt :: MultiLine -> Repl String
prompt = \case
  MultiLine -> pure ":"
  SingleLine ->
    get >>= \case
      Just pf -> pure "prooving > "
      Nothing -> pure ">>> "

cmd :: String -> Repl ()
cmd s = do
  st <- get
  maybe handleNewProof handleTactic st $ pack s
 where
  handleTactic pf = parseTactic "test" >|> flip evalTactic pf
  handleNewProof = parseProof "theorem" >|> check >|> (updateProof . proof emptyEnv . fst)
  check (InitProof e) = typecheck e

help :: String -> Repl ()
help args = liftIO $ putStrLn $ "Help: " ++ args

opts :: [(String, String -> Repl ())]
opts =
  [ ("help", help)
  , ("test", test . pack)
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

printP :: Pretty a => a -> IO ()
printP = putStrLn . renderP
printError :: Pretty a => a -> IO ()
printError a = do
  setSGR [SetColor Foreground Vivid Red]
  putStr $ renderP a
  setSGR [SetColor Foreground Vivid White]
  putStrLn ""

updateProof :: Proof -> Repl ()
updateProof pf = put (Just pf) >> liftIO (displayProof pf)

evalTactic :: Tactic -> Proof -> Repl ()
evalTactic = eval
 where
  eval (Intro n) = updatePF . intro n
  eval (Apply n) = updatePF . applyAssm n
  eval (Rewrite n) = error "not implemented"
  eval Qed = (`run` finish) . qed
  run :: Proove a -> (a -> Repl ()) -> Repl ()
  run pv f = runExcept pv |>> f
  updatePF = flip run updateProof
  finish :: Pretty a => a -> Repl ()
  finish t = put Nothing >> liftIO (putStr "Proof: " >> printP t)

test :: Text -> Repl ()
test = parseExpr "test" >|> typecheck >|> (liftIO . printP . fst)

displayProof :: Proof -> IO ()
displayProof pf = do
  times <- liftIO height
  liftIO $ putStrLn $ replicate times '\n'
  liftIO $ printP pf
 where
  height = do
    i <- getTerminalSize
    case i of
      Nothing -> return 10
      Just (h, _) -> return h