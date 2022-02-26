module Main where

import qualified Ast
import Control.Monad
import qualified Data.Text.IO as T
import Proof
import TacticParser
import Typechecker
import Types

import Control.Monad.Except
import Parser (parseExpr)
import Pretty
import System.Console.ANSI
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (isEOF)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["proove"] -> proove
    ["test"] -> test
    _ -> putStr "Invalid argument. Valid ones are \"proove\" and \"test\". " >> exitFailure

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

test :: IO ()
test = do
  t <- T.getLine
  case parseExpr "test" t of
    Left e -> printError e
    Right e -> do
      printP e
      case typecheck e of
        Left e -> printError e
        Right (t, tp) -> printP tp
  test

proove :: IO ()
proove = do
  putStr ">> "
  t <- T.getLine
  case parseProof "proof" t of
    Left err -> printError err
    Right (InitProof e) -> case typecheck e of
      Left txt -> putStrLn $ renderP txt
      Right (t, tp) -> do
        loop (proof emptyEnv t)

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
