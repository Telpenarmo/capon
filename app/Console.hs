module Console (
    (|>>),
    (>|>),
    renderOut,
    renderString,
    withColor,
    withDull,
    printError,
    printSuccess,
    confirm,
) where

import Control.Monad ((<=<), (>=>))
import Control.Monad.Except (MonadIO, liftIO)
import Data.Char (showLitChar, toLower)
import Data.List (isPrefixOf)
import Data.Text (unpack)
import Prettyprinter (Doc, LayoutOptions (LayoutOptions), PageWidth (AvailablePerLine), SimpleDocStream, annotate, defaultLayoutOptions, hardline, layoutPretty, layoutSmart)
import qualified Prettyprinter.Render.String as PS
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, colorDull, renderIO, renderLazy, renderStrict)
import System.Console.ANSI (getTerminalSize)
import System.IO (stdout)

import Capon.Pretty (Pretty (pretty))

(|>>) :: (Pretty e, MonadIO m) => Either e a -> (a -> m ()) -> m ()
e |>> cont = either (liftIO . printError) cont e

infixr 1 >|>
(>|>) :: (Pretty e, MonadIO m) => (t -> Either e a) -> (a -> m ()) -> t -> m ()
f >|> g = \x -> f x |>> g

layout :: Doc ann -> IO (SimpleDocStream ann)
layout doc = flip layoutSmart doc <$> options
  where
    options =
        LayoutOptions
            . uncurry AvailablePerLine
            . getSize
            . maybe 80 snd
            <$> getTerminalSize
    getSize :: Int -> (Int, Double)
    getSize = \case
        n | n <= 60 -> (n, 1.0)
        n | n <= 80 -> (n, 60.0 / fromIntegral n)
        n -> (80, 60.0 / fromIntegral n)

renderString :: Doc AnsiStyle -> IO String
renderString doc = unpack . renderStrict <$> layout (doc <> "\STX")

renderOut :: Doc AnsiStyle -> IO ()
renderOut = (layout >=> renderIO stdout) . (<> hardline)

withColor, withDull :: Color -> Doc AnsiStyle -> Doc AnsiStyle
withColor c = annotate (color c) . ("\STX" <>)
withDull c = annotate (colorDull c) . ("\STX" <>)

printError, printSuccess :: Pretty a => a -> IO ()
printError = renderOut . withColor Red . pretty
printSuccess = renderOut . withColor Green . pretty

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