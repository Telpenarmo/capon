{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import qualified Ast
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
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

(|>>) :: (Pretty e, MonadIO m) => Either e a -> (a -> m ()) -> m ()
e |>> cont = either (liftIO . printError) cont e

infixr 1 >|>
(>|>) :: (Pretty e, MonadIO m) => (t -> Either e a) -> (a -> m ()) -> t -> m ()
f >|> g = \x -> f x |>> g

prompt :: MultiLine -> Repl String
prompt = \case
  MultiLine -> pure ":"
  SingleLine -> get >>= pure <$> maybe ">>> " (const "prooving > ")

cmd :: String -> Repl ()
cmd s = do
  st <- get
  maybe handleNewProof handleTactic st $ pack s
 where
  handleTactic pf = parseTactic "tactic" >|> flip evalTactic pf
  handleNewProof = parseProof "theorem" >|> check >|> (updateState . proof emptyEnv . fst)
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

printColored :: Pretty a => Color -> a -> IO ()
printColored c x = do
  setSGR [SetColor Foreground Vivid c]
  putStr $ renderP x
  setSGR [SetColor Foreground Vivid White]
  putStrLn ""

printError :: Pretty a => a -> IO ()
printError = printColored Red
printSuccess :: Pretty a => a -> IO ()
printSuccess = printColored Green

updateState :: Proof -> Repl ()
updateState pf = do
  put $ Just pf
  liftIO $ displayProof pf

evalTactic :: Tactic -> Proof -> Repl ()
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

test :: Text -> Repl ()
test = parseExpr "test" >|> typecheck >|> (liftIO . printP . fst)

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
      Just (h, _) -> return h