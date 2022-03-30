module Context (
    Context,
    empty,
    insertAbstract,
    insertDefined,
    lookupType,
    lookupDefinition,
    toList
) where

import qualified Data.Map as Map
import Data.Text (Text)

newtype Context a = Context {getContext :: Map.Map Text (a, Maybe a)}

empty :: Context a
empty = Context Map.empty

insertAbstract :: Text -> a -> Context a -> Context a
insertAbstract k v (Context ctx) = Context $ Map.insert k (v, Nothing) ctx

insertDefined :: Text -> a -> a -> Context a -> Context a
insertDefined k def tp (Context ctx) = Context $ Map.insert k (tp, Just def) ctx

lookupType :: Text -> Context a -> Maybe a
lookupType k (Context ctx) = fst <$> Map.lookup k ctx

lookupDefinition :: Text -> Context a -> Maybe a
lookupDefinition k (Context ctx) = Map.lookup k ctx >>= snd

toList :: Context a -> [(Text, (a, Maybe a))]
toList (Context ctx) = Map.toList ctx