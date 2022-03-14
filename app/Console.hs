module Console where

import Control.Monad.Except (MonadIO, liftIO)
import Data.Char (toLower)
import Data.List (isPrefixOf)
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

confirm :: Bool -> IO Bool
confirm deflt = do
    let (y, n) = if deflt then ("Y", "n") else ("y", "N")
    putStrLn $ " [" ++ y ++ "/" ++ n ++ "]"
    ans <- getLine
    return $ case fmap toLower ans of
        "" -> deflt
        ans | ans `isPrefixOf` "yes" -> True
        ans | ans `isPrefixOf` "no" -> False
        _ -> False