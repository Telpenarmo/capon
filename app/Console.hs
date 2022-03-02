module Console where

import Control.Monad.Except (MonadIO, liftIO)
import Pretty (Pretty, renderP)
import System.Console.ANSI

(|>>) :: (Pretty e, MonadIO m) => Either e a -> (a -> m ()) -> m ()
e |>> cont = either (liftIO . printError) cont e

infixr 1 >|>
(>|>) :: (Pretty e, MonadIO m) => (t -> Either e a) -> (a -> m ()) -> t -> m ()
f >|> g = \x -> f x |>> g

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