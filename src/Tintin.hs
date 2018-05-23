module Tintin
  ( runApp
  )
where

import Tintin.Core

import qualified Tintin.Capabilities.Logging as Logging
import qualified Tintin.Capabilities.Filesystem as Filesystem (Capability(..), getPathsWith, Extension(..), Path(..), deleteIfExists, currentDirectory, list)
import qualified Tintin.Capabilities.Process as Process
import qualified Tintin.Parse as Parse
import qualified Tintin.Render as Render
import qualified Tintin.ConfigurationLoading as ConfigurationLoading


runApp :: ( Has Logging.Capability eff
          , Has Filesystem.Capability eff
          , Has Process.Capability eff
          )
       => OutputDirectory
       -> Effectful eff ()
runApp outputDirectory = do
  cleanUp outputDirectory
  docDir    <- getDocumentationDirectory
  filenames <- getDocumentationFilenames docDir

  Parse.docs docDir filenames
   >>= Render.perform
   >>= ConfigurationLoading.loadInfo
   >>= Render.writeOutput outputDirectory


cleanUp :: ( Has Logging.Capability eff
           , Has Filesystem.Capability eff
           )
        => OutputDirectory
        -> Effectful eff ()
cleanUp (OutputDirectory p) = do
  Logging.debug "Cleaning output directory"
  Filesystem.deleteIfExists (Filesystem.Path p)


getDocumentationFilenames :: ( Has Logging.Capability eff
                             , Has Filesystem.Capability eff
                             )
                          => DocumentationDirectory
                          -> Effectful eff [Filesystem.Path]
getDocumentationFilenames (DocumentationDirectory docDir) = do
  Logging.debug ( "Reading documentation files at " <> docDir )
  Filesystem.getPathsWith ( Filesystem.Extension ".md") <$> Filesystem.list (Filesystem.Path docDir)


getDocumentationDirectory :: Has Filesystem.Capability eff
                          => Effectful eff DocumentationDirectory
getDocumentationDirectory = do
  Filesystem.Path currentDir <- Filesystem.currentDirectory
  return ( DocumentationDirectory $ currentDir <> "/doc/" )



