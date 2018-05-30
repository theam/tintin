module Main where

import Universum

import Options.Generic

import Tintin
import Tintin.Core
import qualified Tintin.Capabilities.Logging as Logging
import qualified Tintin.Capabilities.Filesystem as Filesystem
import qualified Tintin.Capabilities.Process as Process


data Options
  = Run
    { outputDirectory :: Maybe Text
    , verbose  :: Bool
    , runghc :: Bool
    }
  | Publish
    { verbose :: Bool
    , documentationDirectory :: Maybe Text
    }

deriving instance Generic Options
instance ParseRecord Options


main :: IO ()
main = do
  opts <- getRecord "Tintin, the tutorial website generator"
  case opts of
    Run outputDirectory verbose shouldUseCabal -> do
      let outputDir = fromMaybe ".stack-work/tintin/rendered/" outputDirectory
      let logger     = if verbose
                       then Logging.stdOut
                       else Logging.mute
      let filesystem = Filesystem.local
      let process    = Process.local
      runEffects ( runApp shouldUseCabal $ OutputDirectory outputDir ) (logger, filesystem, process)

    Publish verbose documentationDirectory -> do
      let outputDir = fromMaybe ".stack-work/tintin/rendered/" documentationDirectory
      let logger     = if verbose
                       then Logging.stdOut
                       else Logging.mute
      let filesystem = Filesystem.local
      let process    = Process.local
      runEffects ( publish $ OutputDirectory outputDir ) (logger, filesystem, process)

