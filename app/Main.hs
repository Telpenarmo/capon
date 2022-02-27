module Main where

import qualified Ast
import Control.Monad
import qualified Data.Text.IO as T
import Proof
import TacticParser
import Typechecker
import Types

import Control.Monad.Except
import Data.List (isPrefixOf)
import Data.Text (Text, pack)
import Parser (parseExpr)
import Pretty
import System.Console.ANSI
import System.Console.Repline
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (isEOF)

type Repl a = HaskelineT IO a

cmd :: String -> Repl ()
cmd = proove . pack

help :: String -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

opts :: Options (HaskelineT IO)
opts =
  [ ("help", help)
  , ("test", test . pack)
  ]

completer :: Monad m => WordCompleter m
completer n = return []

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to Capon!"

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

main :: IO ()
main =
  evalReplOpts $
    ReplOpts
      { banner = const (pure ">>> ")
      , command = cmd
      , options = opts
      , prefix = Just ':'
      , multilineCommand = Nothing
      , tabComplete = Word0 completer
      , initialiser = ini
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

eval :: Tactic -> Proof -> Either (Proove Proof) (Proove Term)
eval (Intro n) = Left <$> intro n
eval (Apply n) = Left <$> applyAssm n
eval (Rewrite n) = Left <$> error "not implemented"
eval Qed = Right <$> qed

test :: Text -> Repl ()
test s = do
  case parseExpr "test" s of
    Left e -> liftIO $ printError e
    Right e -> do
      liftIO $ printP e
      case typecheck e of
        Left e -> liftIO $ printError e
        Right (t, tp) -> liftIO $ printP tp

proove :: Text -> Repl ()
proove t = do
  case parseProof "proof" t of
    Left err -> liftIO $ printError err
    Right (InitProof e) -> case typecheck e of
      Left txt -> liftIO $ putStrLn $ renderP txt
      Right (t, tp) -> do
        liftIO $ loop (proof emptyEnv t)

loop :: Proof -> IO ()
loop pf = do
  times <- height
  putStrLn $ replicate times '\n'
  printP pf

  t <- T.getLine
  case parseTactic "test" t of
    Left err -> printError err
    Right tac ->
      case eval tac pf of
        Left pm -> case runExcept pm of
          Left err -> printError err
          Right pr -> loop pr
        Right tm -> case runExcept tm of
          Left err -> printError err
          Right t -> print "Proof: " >> printP t
 where
  height = do
    i <- getTerminalSize
    case i of
      Nothing -> return 10
      Just (h, _) -> return h
