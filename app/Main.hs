module Main where

import Universum

import Options.Generic

import Tintin
import Tintin.Core
import qualified Tintin.Capabilities.Logging as Logging
import qualified Tintin.Capabilities.Filesystem as Filesystem


data Options = Options
  { outputDirectory :: Maybe Text
  , verbose :: Bool
  }

deriving instance Generic Options
instance ParseRecord Options


main :: IO ()
main = do
  opts <- getRecord "Tintin, the tutorial website generator"
  let outputDir = fromMaybe "dist/tintin/" (outputDirectory opts)
  let logger = if verbose opts
               then Logging.stdOut
               else Logging.mute
  let filesystem = Filesystem.local
  runEffects ( runApp $ OutputDirectory outputDir ) (logger, filesystem)
