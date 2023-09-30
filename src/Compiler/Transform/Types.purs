module Compiler.Transform.Types (
  Module,
  Imports(..),
  Bindings(..),
  Binding(..),
  Func,
  Expr(..),
  Literal(..),
  Variable,
  AppType(..),
  Accessor(..),
  Primop(..)
) where

import Prelude

import Data.Array (nub)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))

--
--
--

type Module = {
  name :: String,
  imports :: Imports,
  bindings :: Bindings
}

--
--
--

newtype Imports = Imports (Array String)

instance Semigroup Imports where
  append (Imports b1) (Imports b2) =
    Imports (nub $ b1 <> b2)

instance Monoid Imports where
  mempty = Imports []

--
--
--

newtype Bindings = Bindings (Array Binding)

instance Semigroup Bindings where
  append (Bindings b1) (Bindings b2) =
    Bindings (b1 <> b2)

instance Monoid Bindings where
  mempty = Bindings []

--
--
--

data Binding
  = Var String Expr
  | Def String Func

--
--
--

type Func = {
  args :: Array Expr,
  locals :: Array Binding,
  return :: Expr
}

--
--
--

data Expr
  = Literal Literal
  | Variable Variable
  | App AppType Expr (Array Expr)
  | Abs Func
  | Accessor Expr Accessor
  | IfElse (Array (Expr /\ Expr)) Expr
  | Primop Primop
  | Unimplemented String
  | Skip

--
--
--

data Literal
  = LitInt Int
  | LitFloat Number
  | LitString String
  | LitRune Char
  | LitBool Boolean
  | LitArray (Array Expr)
  | LitRecord (Array (String /\ Expr))

--
--
--

type Variable = {
  mod :: Maybe String,
  name :: String
}

--
--
--

data AppType
  = Uncurried
  | Curried

--
--
--

data Accessor
  = Prop String
  | Index Int
  | CtorField String

--
--
--

data Primop
  = Unary String Expr
  | UnarySuffix String Expr
  | Binary String Expr Expr
  | ArrayIndex Expr Expr
  | Ctor String (Array String)
