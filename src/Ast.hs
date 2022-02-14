module Ast where

import Data.Text
import Data.Text.Encoding

type Var = Text
newtype Binding = Bind (Text, Expr)
data Sort = Type Int | Prop

type Expr = (ExprData, Location)

data ExprData
    = Sort Sort
    | Var Var
    | Lambda Binding Expr
    | ForAll Binding Expr
    | App Expr Expr
    | LetIn Binding Expr Expr

data Location = Location {line :: Int, column :: Int, file :: Text}