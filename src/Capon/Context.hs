{-# LANGUAGE DeriveFunctor #-}

module Capon.Context (
    Context,
    empty,
    insertAbstract,
    insertDefined,
    lookupType,
    lookupDefinition,
    toList,
) where

import qualified Data.Map as Map
import Data.Text (Text)

newtype X a = X (Map.Map Text (a, Maybe a)) deriving (Functor)

data Context a = Context {getContext :: X a, mapper :: Text -> a -> a}

empty :: (Text -> a -> a) -> Context a
empty = Context $ X Map.empty

insertAbstract :: Text -> a -> Context a -> Context a
insertAbstract k v (Context (X ctx) f) = Context (f k <$> X (Map.insert k (v, Nothing) ctx)) f

insertDefined :: Text -> a -> a -> Context a -> Context a
insertDefined k def tp (Context (X ctx) f) = Context (f k <$> X (Map.insert k (tp, Just def) ctx)) f

lookupType :: Text -> Context a -> Maybe a
lookupType k (Context (X ctx) f) = fst <$> Map.lookup k ctx

lookupDefinition :: Text -> Context a -> Maybe a
lookupDefinition k (Context (X ctx) f) = Map.lookup k ctx >>= snd

toList :: Context a -> [(Text, (a, Maybe a))]
toList (Context (X ctx) f) = Map.toList ctx