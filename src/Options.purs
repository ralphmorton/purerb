module Options (
  Options
) where

import Node.Path (FilePath)

type Options = {
  srcDir :: FilePath,
  coreFnDir :: FilePath,
  outputDir :: FilePath,
  printUnimplemented :: Boolean
}
