module Compiler.Transform (
  transform,
  toModulePath
) where

import Prelude

import Control.Monad.State (State, execState, gets, modify_)
import Data.Array (concatMap, fromFoldable, head, snoc, uncons, unsnoc, reverse)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (foldl, traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.String (replace, split, toLower)
import Data.String as Str
import Data.String.CodeUnits as S
import Data.Traversable (traverse)
import Data.Tuple (Tuple, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.Backend.Optimizer.Convert (BackendBindingGroup, BackendModule)
import PureScript.Backend.Optimizer.CoreFn (Ident, Literal(..), ModuleName, Qualified(..), Prop(..), ProperName(..))
import PureScript.Backend.Optimizer.Semantics (NeutralExpr(..))
import PureScript.Backend.Optimizer.Syntax (
  BackendAccessor(..),
  BackendOperator(..),
  BackendOperator1(..),
  BackendOperator2(..),
  BackendOperatorNum(..),
  BackendOperatorOrd(..),
  BackendSyntax(..),
  Level,
  Pair(..)
)

import Compiler.Transform.Types as Syn
import Options (Options)

--
--
--

type St = {
  opts :: Options,
  mod :: BackendModule,
  out :: Syn.Module
}

type Transform a = State St a

transform :: Options -> BackendModule -> Syn.Module
transform opts mod = _.out $ execState transform' {
  opts,
  mod,
  out: {
    name: toModuleName mod.name,
    imports: mempty,
    bindings: mempty
  }
}

toModuleName :: ModuleName -> String
toModuleName = ("::" <> _) <<< replace (wrap ".") (wrap "::") <<< unwrap

--
--
--

transform' :: Transform Unit
transform' = do
  imports
  bindings

--
--
--

imports :: Transform Unit
imports = do
  mod <- gets _.mod
  traverse_ (pushImport <<< toModulePath) mod.imports

toModulePath :: ModuleName -> String
toModulePath =
  Str.joinWith "/" <<<
  map snakeCase <<<
  Str.split (wrap ".") <<<
  unwrap

snakeCase :: String -> String
snakeCase = combine <<< foldl transform { init: "", tail: [] } <<< S.toCharArray
  where
  combine { init, tail } = init <> Str.toLower (S.fromCharArray tail)

  transform { init, tail } c = case isUpper c of
    false ->
      {
        init,
        tail: snoc tail c
      }
    true -> case unsnoc tail of
      Just unsnoced | not isUpper unsnoced.last ->
        {
          init: case init of
            "" -> Str.toLower (S.fromCharArray tail)
            _ -> init <> "_" <> Str.toLower (S.fromCharArray tail),
          tail: [c]
        }
      _ ->
        {
          init,
          tail: snoc tail c
        }

isUpper :: Char -> Boolean
isUpper = case _ of
  'A' -> true
  'B' -> true
  'C' -> true
  'D' -> true
  'E' -> true
  'F' -> true
  'G' -> true
  'H' -> true
  'I' -> true
  'J' -> true
  'K' -> true
  'L' -> true
  'M' -> true
  'N' -> true
  'O' -> true
  'P' -> true
  'Q' -> true
  'R' -> true
  'S' -> true
  'T' -> true
  'U' -> true
  'V' -> true
  'W' -> true
  'X' -> true
  'Y' -> true
  'Z' -> true
  _ -> false

--
--
--

bindings :: Transform Unit
bindings = do
  groups <- gets _.mod.bindings
  traverse_ bindingGroup groups

bindingGroup :: BackendBindingGroup Ident NeutralExpr -> Transform Unit
bindingGroup = traverse_ (uncurry binding) <<< _.bindings

binding :: Ident -> NeutralExpr -> Transform Unit
binding ident e@(NeutralExpr syn) = do
  let name = symbol ident
  case syn of
    Abs ax e -> do
      fn <- func ax e
      pushBinding (Syn.Def name fn)
    _ -> do
      ex <- expr e
      pushBinding (Syn.Var name ex)

--
--
--

expr :: NeutralExpr -> Transform Syn.Expr
expr (NeutralExpr syn) = case syn of
  Var (Qualified name ident) -> do
    mod <- gets _.mod
    case name of
      Just ident' | ident' /= mod.name ->
        pure $ Syn.Variable {
          mod: pure (toModuleName ident'),
          name: symbol ident
        }
      _ ->
        pure $ Syn.Variable {
          mod: pure (toModuleName mod.name),
          name: symbol ident
        }
  Local ident _ -> case ident of
    Nothing ->
      pure $ Syn.Variable {
        mod: Nothing,
        name: "_"
      }
    Just id ->
      pure $ Syn.Variable {
        mod: Nothing,
        name: symbol id
      }
  Lit lit -> do
    l <- literal lit
    pure $ Syn.Literal l
  App e ax -> do
    ex <- expr e
    args <- traverse expr (fromFoldable ax)
    pure $ Syn.App Syn.Curried ex args
  Abs ax e ->
    Syn.Abs <$> func ax e
  UncurriedApp ex args ->
    pure $ Syn.Unimplemented "uncurriedapp"
  UncurriedAbs args ex ->
    pure $ Syn.Unimplemented "uncurriedabs"
  UncurriedEffectApp ex args ->
    pure $ Syn.Unimplemented "uncurriedeffectapp"
  UncurriedEffectAbs args ex ->
    pure $ Syn.Unimplemented "uncurriedeffectabs"
  Accessor e a -> do
    ex <- expr e
    let acc = accessor a
    pure $ Syn.Accessor ex acc
  Update ex props ->
    pure $ Syn.Unimplemented "update"
  CtorSaturated (Qualified mod ident) _ _ _ ax -> do
    let fn = Syn.Variable { mod: toModuleName <$> mod, name: symbol ident }
    args <- traverse expr (snd <$> ax)
    pure $ Syn.App Syn.Curried fn args
  CtorDef _ _ ident params ->
    pure case params of
      [] ->
        construct ident params
      _ ->
        ctor params ident params
  LetRec level args ex ->
    pure$ Syn.Unimplemented "letrec"
  Let ident1 level ex1 ex2 ->
    pure $ Syn.Unimplemented "let"
  EffectBind ident1 level ex1 ex2 ->
    pure $ Syn.Unimplemented "effectbind"
  EffectPure ex ->
    pure $ Syn.Unimplemented "effectpure"
  EffectDefer ex ->
    pure $ Syn.Unimplemented "effectdefer"
  Branch bx e -> do
    branches <- traverse branch (fromFoldable bx)
    ex <- expr e
    pure $ Syn.IfElse branches ex
  PrimOp op ->
    primop op
  PrimEffect eff ->
    pure $ Syn.Unimplemented "primeffect"
  PrimUndefined ->
    pure $ Syn.Unimplemented "primundefined"
  Fail e ->
    pure $
      Syn.App
        Syn.Curried
        (Syn.Variable { mod: Nothing, name: "raise" })
        [Syn.Literal $ Syn.LitString e]

--

literal :: Literal NeutralExpr -> Transform Syn.Literal
literal = case _ of
  LitInt v ->
    pure $ Syn.LitInt v
  LitNumber v ->
    pure $ Syn.LitFloat v
  LitString v ->
    pure $ Syn.LitString v
  LitChar v ->
    pure $ Syn.LitRune v
  LitBoolean v ->
    pure $ Syn.LitBool v
  LitArray e -> do
    ex <- traverse expr e
    pure $ Syn.LitArray ex
  LitRecord p -> do
    px <- traverse prop p
    pure $ Syn.LitRecord px

prop :: Prop NeutralExpr -> Transform (String /\ Syn.Expr)
prop (Prop name e) = do
  ex <- expr e
  pure $ symbol (wrap name) /\ ex


--

func :: (NonEmptyArray (Tuple (Maybe Ident) Level)) -> NeutralExpr -> Transform Syn.Func
func ax e = do
  ex <- expr e
  let (bx /\ ret) = funcBody ex
  pure {
    args: uncurry arg <$> fromFoldable ax,
    locals: bx,
    return: ret
  }

arg :: Maybe Ident -> Level -> Syn.Expr
arg ident _ = case ident of
  Nothing ->
    Syn.Variable { mod: Nothing, name: "_" }
  Just id ->
    Syn.Variable { mod: Nothing, name: symbol id }

funcBody :: Syn.Expr -> Array Syn.Binding /\ Syn.Expr
funcBody exp = case exp of
  Syn.App typ e ax ->
    let
      res = foldl buildLocals { ix: 0, bindings: [], args: [] } ax
    in
    res.bindings /\ Syn.App typ e res.args
  _ ->
    [] /\ exp

type BuildLocals = {
  ix :: Int,
  bindings :: Array Syn.Binding,
  args :: Array Syn.Expr
}

buildLocals :: BuildLocals -> Syn.Expr -> BuildLocals
buildLocals st exp = case exp of
  Syn.IfElse _ _ ->
    let
      name = "__pgrloc_" <> show st.ix
      args = [Syn.Variable { mod: Nothing, name: "v" }]
      b = Syn.Var name $ Syn.Abs { args, locals: [], return: exp }
      a = Syn.App Syn.Curried (Syn.Variable { mod: Nothing, name }) [Syn.Literal $ Syn.LitInt 1]
    in
    st {
      ix = st.ix + 1,
      bindings = st.bindings <> [b],
      args = st.args <> [a]
    }
  _ ->
    st { args = st.args <> [exp] }

--

accessor :: BackendAccessor -> Syn.Accessor
accessor = case _ of
  GetProp name ->
    Syn.Prop (symbol $ wrap name)
  GetIndex ix ->
    Syn.Index ix
  GetCtorField _ _ _ _ name _ ->
    Syn.CtorField name

--

construct :: Ident -> Array String -> Syn.Expr
construct ident params = Syn.Primop $ Syn.Ctor (unwrap ident) params

ctor :: Array String -> Ident -> Array String -> Syn.Expr
ctor originalParams ident params = case uncons params of
  Nothing ->
    construct ident originalParams
  Just { head, tail } ->
    Syn.Abs {
      args: [Syn.Variable { mod: Nothing, name: head }],
      locals: [],
      return: ctor originalParams ident tail
    }

--

branch :: Pair NeutralExpr -> Transform (Syn.Expr /\ Syn.Expr)
branch (Pair e1 e2) = do
  ex1 <- expr e1
  ex2 <- expr e2
  pure $ ex1 /\ ex2

--

primop :: BackendOperator NeutralExpr -> Transform Syn.Expr
primop bOp = case bOp of
  Op1 op ex -> case op of
    OpBooleanNot ->
      unaryOp (Syn.Unary "!") ex
    OpIntBitNot ->
      unaryOp (Syn.Unary "~") ex
    OpIntNegate ->
      unaryOp (Syn.Unary "-") ex
    OpNumberNegate ->
      unaryOp (Syn.Unary "-") ex
    OpArrayLength ->
      unaryOp (Syn.UnarySuffix ".size") ex
    OpIsTag (Qualified _ ident) ->
      unaryOp (Syn.UnarySuffix ("[:tag]==\"" <> unwrap ident <> "\"")) ex
  Op2 op ex1 ex2 -> case op of
    OpArrayIndex ->
      binOp Syn.ArrayIndex ex1 ex2
    OpBooleanAnd ->
      binOp (Syn.Binary "&&") ex1 ex2
    OpBooleanOr ->
      binOp (Syn.Binary "||") ex1 ex2
    OpBooleanOrd f ->
      binOp (Syn.Binary (ordOp f)) ex1 ex2
    OpCharOrd f ->
      binOp (Syn.Binary (ordOp f)) ex1 ex2
    OpIntBitAnd ->
      binOp (Syn.Binary "&") ex1 ex2
    OpIntBitOr ->
      binOp (Syn.Binary "|") ex1 ex2
    OpIntBitShiftLeft ->
      binOp (Syn.Binary "<<") ex1 ex2
    OpIntBitShiftRight ->
      binOp (Syn.Binary ">>") ex1 ex2
    OpIntBitXor ->
      binOp (Syn.Binary "^") ex1 ex2
    OpIntBitZeroFillShiftRight ->
      pure $ Syn.Unimplemented "Logical bitshift not implemented"
    OpIntNum f ->
      binOp (Syn.Binary (numOp f)) ex1 ex2
    OpIntOrd f ->
      binOp (Syn.Binary (ordOp f)) ex1 ex2
    OpNumberNum f ->
      binOp (Syn.Binary (numOp f)) ex1 ex2
    OpNumberOrd f ->
      binOp (Syn.Binary (ordOp f)) ex1 ex2
    OpStringAppend ->
      binOp (Syn.Binary "+") ex1 ex2
    OpStringOrd f ->
      binOp (Syn.Binary (ordOp f)) ex1 ex2

unaryOp :: (Syn.Expr -> Syn.Primop) -> NeutralExpr -> Transform Syn.Expr
unaryOp f e = do
  ex <- expr e
  pure $ Syn.Primop (f ex)

binOp :: (Syn.Expr -> Syn.Expr -> Syn.Primop) -> NeutralExpr -> NeutralExpr -> Transform Syn.Expr
binOp f e1 e2 = do
  ex1 <- expr e1
  ex2 <- expr e2
  pure $ Syn.Primop (f ex1 ex2)

ordOp :: BackendOperatorOrd -> String
ordOp = case _ of
  OpEq ->
    "=="
  OpNotEq ->
    "!="
  OpGt ->
    ">"
  OpGte ->
    ">="
  OpLt ->
    "<"
  OpLte ->
    "<="

numOp :: BackendOperatorNum -> String
numOp = case _ of
  OpAdd ->
    "+"
  OpDivide ->
    "/"
  OpMultiply ->
    "*"
  OpSubtract ->
    "-"

--
--
--

pushImport :: String -> Transform Unit
pushImport name = modify_ $ withModule \p -> p {
  imports = p.imports <> Syn.Imports [name]
}

pushBinding :: Syn.Binding -> Transform Unit
pushBinding b = modify_ $ withModule \p -> p {
  bindings = p.bindings <> Syn.Bindings [b]
}

withModule :: (Syn.Module -> Syn.Module) -> St -> St
withModule f st = st { out = f st.out }

--
--
--

symbol :: Ident -> String
symbol = unwrap
