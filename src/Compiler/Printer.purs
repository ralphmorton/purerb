module Compiler.Printer (
  print
) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Reader.Class (ask)
import Data.Array (fromFoldable, intersperse, uncons)
import Data.Either (Either)
import Data.Foldable (class Foldable, fold)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.String (split)
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\))
import Dodo (Doc, break, indent, lines, text)

import Compiler.Transform.Types (
  Accessor(..),
  Binding(..),
  Bindings(..),
  Expr(..),
  Func,
  Imports(..),
  Literal(..),
  Module,
  AppType(..),
  Primop(..)
)
import Options (Options)

--
--
--

type Printer' a = ExceptT String (Reader Options) a
type Printer = Printer' (Doc Void)

print :: Options -> Module -> String -> Either String (Doc Void)
print opts mod foreigns = flip runReader opts $ runExceptT $ do 
  bx <- bindings mod.bindings
  pure $ lines [
    imports mod.imports,
    text ("module " <> mod.name),
    indent bx,
    indent $ lines $ text <$> split (wrap "\n") foreigns,
    text "end",
    break
  ]

--
--
--

imports :: Imports -> Doc Void
imports (Imports ix) = lines $ ix <#> \i ->
  text ("require '" <> i <> "'")

--
--
--

bindings :: Bindings -> Printer
bindings (Bindings bx) = lines <$> traverse binding bx

binding :: Binding -> Printer
binding = case _ of
  Var name e -> do
    ex <- expr e
    pure $ lines [
      text ("def self." <> name),
      indent ex,
      text "end"
    ]
  Def name f -> do
    fn <- func Method f
    pure $ lines [
      text ("def self." <> name),
      fn
    ]

expr :: Expr -> Printer
expr = case _ of
  Literal lit ->
    literal lit
  Variable { mod, name } ->
    pure $ text $ maybe "" (_ <> ".") mod <> name
  App typ e ax ->
    app typ e ax
  Abs f ->
    func Lambda f
  Accessor e acc ->
    accessor e acc
  IfElse bx def ->
    ifElse bx def
  Primop op ->
    primop op
  Unimplemented e -> do
    { printUnimplemented } <- ask
    case printUnimplemented of
      true ->
        pure $ text $ "raise(\"UNIMPLEMENTED: " <> e <> "\")"
      false ->
        throwError e
  Skip ->
    pure mempty

--

literal :: Literal -> Printer
literal = case _ of
  LitInt v ->
    pure $ text (show v)
  LitFloat v ->
    pure $ text (show v)
  LitString v ->
    pure $ text (show v)
  LitRune v ->
    pure $ text (show v)
  LitBool v ->
    pure $ text (show v)
  LitArray v -> do
    vx <- traverse expr v
    pure $ text "[" <> commas vx <> text "]"
  LitRecord p -> do
    px <- traverse (uncurry prop) p
    pure $ text "{" <> commas px <> text "}"

prop :: String -> Expr -> Printer
prop name e = do
  ex <- expr e
  pure $ text (name <> ":") <> ex

--
--
--

app :: AppType -> Expr -> Array Expr -> Printer
app typ e ax = do
  ex <- expr e
  ax' <- args typ ax
  pure $ ex <> ax'


args :: AppType -> Array Expr -> Printer
args typ ex = case typ of
  Curried ->
    fold <$> traverse arg ex
  Uncurried -> do
    ex' <- commas <$> traverse expr ex
    pure $ text ".call(" <> ex' <> text ")"

arg :: Expr -> Printer
arg e = do
  ex <- expr e
  pure $ text ".call(" <> ex <> text ")"

--
--
--

accessor :: Expr -> Accessor -> Printer
accessor e acc = do
  ex <- expr e
  case acc of
    Prop name ->
      pure $ ex <> text ("[:" <> name <> "]")
    Index ix ->
      pure $ ex <> text ("[" <> show ix <> "]")
    CtorField name ->
      pure $ ex <> text ("[:params][:" <> name <> "]")

--
--
--

ifElse :: Array (Expr /\ Expr) -> Expr -> Printer
ifElse bx def = do
  branches <- traverse (uncurry branch) bx
  default <- expr def
  pure $ lines [
    text "if " <> fold (intersperse (text "elsif ") branches) <> text "else",
    indent default,
    text "end"
  ]

branch :: Expr -> Expr -> Printer
branch con b = do
  condition <- expr con
  body <- expr b
  pure $ fold [
    (text "(" <> condition <> text ")"),
    break,
    indent body,
    break
  ]

--
--
--

primop :: Primop -> Printer
primop = case _ of
  Unary op e -> do
    ex <- expr e
    pure $ text op <> ex
  UnarySuffix op e -> do
    ex <- expr e
    pure $ ex <> text op
  Binary op e1 e2 -> do
    ex1 <- expr e1
    ex2 <- expr e2
    pure $ ex1 <> text op <> ex2
  ArrayIndex e1 e2 -> do
    ex1 <- expr e1
    ex2 <- expr e2
    pure $ ex1 <> text "[" <> ex2 <> text "]"
  Ctor tag params -> do
    let px = commas $ (\p -> text (p <> ":" <> p)) <$> params
    pure $ text "{tag:\"" <> text tag <> text "\",params:{" <> px <> text "}}"

--
--
--

data FuncType
  = Method
  | Lambda

func :: FuncType -> Func -> Printer
func t f = case t of
  Method -> do
    body <- func Lambda f
    pure $ lines [
      indent body,
      text "end"
    ]
  Lambda -> case uncons f.args of
    Nothing -> do
      locals <- traverse binding f.locals
      ret <- expr f.return
      pure $ lines [
        lines locals,
        ret
      ]
    Just { head, tail } -> do
      arg' <- expr head
      body <- func Lambda { args: tail, locals: f.locals, return: f.return }
      pure $ lines [
        (text "lambda do |" <> arg' <> text "|"),
        indent body,
        text "end"
      ]

--
--
--

commas :: forall f. Foldable f => f (Doc Void) -> Doc Void
commas = fold <<< intersperse comma <<< fromFoldable

comma :: Doc Void
comma = text ","
