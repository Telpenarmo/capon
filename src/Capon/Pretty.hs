{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Capon.Pretty (
    Pretty (..),
    pBinding,
    pAbstraction,
    parensIf,
    cite,
    module Prettyprinter
) where

import Control.Monad.Except (runExcept)
import qualified Data.Map as Map
import Data.Text (Text, replicate, unpack)
import Data.Void (Void)
import Prettyprinter hiding (Pretty, pretty)
import qualified Prettyprinter as PP
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

class Pretty a where
    pretty :: a -> Doc ann
    default pretty :: Show a => a -> Doc ann
    pretty = viaShow
    prettyPrec :: Int -> a -> Doc ann
    prettyPrec = const pretty

instance Pretty Text where pretty = PP.pretty
instance Pretty Int where pretty = PP.pretty
instance Pretty String where pretty = PP.pretty

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

cite :: Pretty a => a -> Doc ann
cite = dquotes . pretty

pBinding :: Pretty p => Text -> p -> Doc ann
pBinding x tp = parens $ pretty x PP.<> ":" <+> pretty tp

pAbstraction :: (Pretty p) => Doc ann -> Int -> Text -> p -> p -> Doc ann
pAbstraction symbol p x tp bd =
    sep [symbol <> pBinding x tp, nest 2 $ "→" <+> pretty bd]