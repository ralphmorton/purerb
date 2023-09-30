module Main where

import Prelude

import Control.Monad.Error.Class (throwError, try)
import Data.Array (drop)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console as C
import Effect.Exception (error, message)
import Node.Process (argv, exit')

import Build (build)

main :: Effect Unit
main = do
  res <- try run
  case res of
    Left err -> do
      C.error (message err)
      exit' 1
    Right _ ->
      pure unit

run :: Effect Unit
run = do
  cmd <- drop 2 <$> argv
  case cmd of
    ["build", outputDir] ->
      build {
        srcDir: "./src",
        coreFnDir: "./output",
        outputDir,
        printUnimplemented: true
      }
    _ ->
      throwError $ error $ "Unrecognised command " <> show cmd
