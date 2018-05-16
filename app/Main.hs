module Main where

import Universum

import Options.Generic

import Tintin
import Tintin.Core
import qualified Tintin.Capabilities.Logging as Logging
import qualified Tintin.Capabilities.Filesystem as Filesystem
import qualified Tintin.Capabilities.Process as Process


data Options = Options
  { outputDirectory :: Maybe Text
  , verbose :: Bool
  }

deriving instance Generic Options
instance ParseRecord Options


main :: IO ()
main = do
  opts <- getRecord "Tintin, the tutorial website generator"
  let outputDir = fromMaybe ".stack-work/tintin/rendered/" (outputDirectory opts)
  let logger     = if verbose opts
                   then Logging.stdOut
                   else Logging.mute
  let filesystem = Filesystem.local
  let process    = Process.local
  runEffects ( runApp $ OutputDirectory outputDir ) (logger, filesystem, process)
