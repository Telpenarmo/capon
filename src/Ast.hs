module Ast where

import Data.Text

newtype Var = V Text deriving (Show, Eq, Ord)
newtype Binding = Bind (Text, Expr)
data Sort = Type | Prop deriving (Show)

instance Show Binding where
    show (Bind (n, t)) = unpack n ++ " : " ++ show t

type Expr = (ExprData, Location)

data ExprData
    = Sort Sort
    | Var Var
    | Lambda Binding Expr
    | ForAll Binding Expr
    | App Expr Expr
    | LetIn Binding Expr Expr
    deriving (Show)

data Location = Location {line :: Int, column :: Int, file :: Text}

instance Show Location where
    show loc = ""