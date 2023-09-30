module Build (
  build
) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Parallel (parTraverse)
import Data.Argonaut (parseJson)
import Data.Either (either)
import Data.List as List
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Exception (error)
import Node.Encoding (Encoding(UTF8))
import Node.Glob.Basic (expandGlobs)
import Node.FS.Aff (mkdir, readTextFile, rm')
import Node.Path (FilePath)
import PureScript.Backend.Optimizer.Builder (buildModules)
import PureScript.Backend.Optimizer.CoreFn (Ann, Module)
import PureScript.Backend.Optimizer.CoreFn.Json (decodeModule)
import PureScript.Backend.Optimizer.CoreFn.Sort (sortModules)
import PureScript.Backend.Optimizer.Directives (parseDirectiveFile)
import PureScript.Backend.Optimizer.Directives.Defaults (defaultDirectives)
import PureScript.Backend.Optimizer.Semantics.Foreign (coreForeignSemantics)

import Compiler (compile)
import Options (Options)

build :: Options -> Effect Unit
build = runAff_ (either throwError pure) <<< build'

build' :: Options -> Aff Unit
build' opts = do
  prepareBuildDir opts
  files <- List.fromFoldable <$> expandGlobs opts.coreFnDir ["**/corefn.json"]
  modules <- parTraverse loadModule files
  flip buildModules (sortModules modules) $ {
    directives: (parseDirectiveFile defaultDirectives).directives,
    foreignSemantics: coreForeignSemantics,
    onCodegenModule: compile opts,
    onPrepareModule: const (pure <<< identity),
    traceIdents: Set.empty
  }

prepareBuildDir :: Options -> Aff Unit
prepareBuildDir { outputDir } = do
  rm' outputDir { force: true, maxRetries: 0, recursive: true, retryDelay: 0 }
  mkdir outputDir

loadModule :: FilePath -> Aff (Module Ann)
loadModule path = do
  raw <- readTextFile UTF8 path
  let res = (decodeModule <=< parseJson) raw
  either (throwError <<< error <<< show) pure res
