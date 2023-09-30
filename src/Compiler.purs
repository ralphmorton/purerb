module Compiler (
  compile
) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError, try)
import Data.Array (fromFoldable, snoc, uncons, unsnoc)
import Data.Either (Either(..), either)
import Data.Foldable (foldl, foldM, for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.String (replace, split)
import Dodo (Doc, plainText, twoSpaces)
import Dodo as D
import Effect.Aff (Aff)
import Effect.Exception (error)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (mkdir, readTextFile, writeTextFile)
import Node.Glob.Basic (expandGlobs)
import PureScript.Backend.Optimizer.Builder (BuildEnv)
import PureScript.Backend.Optimizer.Convert (BackendModule, OptimizationSteps)
import PureScript.Backend.Optimizer.CoreFn (Ann, Module, ModuleName)

import Compiler.Printer (print)
import Compiler.Transform (toModulePath, transform)
import Options (Options)

--
--
--

compile :: Options -> BuildEnv -> Module Ann -> BackendModule -> OptimizationSteps -> Aff Unit
compile opts _ _ backend _ = do
  let mod = transform opts backend
  frgn <- readForeign backend.name
  case print opts mod frgn of
    Left e ->
      throwError (error e)
    Right source ->
      writeSource opts backend.name source

--
--
--

readForeign :: ModuleName -> Aff String
readForeign name = map (either (const "") identity) $ try do
  let path = replace (wrap ".") (wrap "/") (unwrap name) <> ".rb"
  readForeignFromSrc path <|> readForeignFromSpagoPackages path

readForeignFromSrc :: String -> Aff String
readForeignFromSrc path = readTextFile UTF8 ("./src/" <> path)

readForeignFromSpagoPackages :: String -> Aff String
readForeignFromSpagoPackages path = do
  matching <- fromFoldable <$> expandGlobs "./.spago/packages" ["**/" <> path]
  case (uncons matching) of
    Nothing ->
      throwError (error "Not found")
    Just { head } ->
      readTextFile UTF8 head

--
--
--

writeSource :: Options -> ModuleName -> Doc Void -> Aff Unit
writeSource { outputDir } name src = do
  let path = outputDir <> "/" <> toModulePath name
  let filename = path <> ".rb"
  ensureDirectory $ split (wrap "/") path
  writeTextFile UTF8 filename $
    D.print plainText twoSpaces src

--
--
--

ensureDirectory :: Array String -> Aff Unit
ensureDirectory parts = for_ (unsnoc parts) \{ init } -> foldM ensure "" init
  where
  ensure prefix dir = do
    let path = if prefix == "" then dir else prefix <> "/" <> dir
    void $ try (mkdir path)
    pure path
