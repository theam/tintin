module Tintin
  ( runApp
  )
where

import Tintin.Core

import qualified Tintin.Capabilities.Logging as Logging
import qualified Tintin.Capabilities.Filesystem as Filesystem
import Tintin.Capabilities.Filesystem (getPathsWith, Extension(..), Path(..), deleteIfExists)
import qualified Tintin.Capabilities.Process as Process
import qualified Tintin.Parse as Parse
import qualified Tintin.Render as Render
import qualified Tintin.ConfigurationLoading as CL


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
   >>= CL.loadInfo
   >>= Render.writeOutput outputDirectory


cleanUp :: ( Has Logging.Capability eff
           , Has Filesystem.Capability eff
           )
        => OutputDirectory
        -> Effectful eff ()
cleanUp (OutputDirectory p) = do
  Logging.debug "Cleaning output directory"
  deleteIfExists (Path p)


getDocumentationFilenames :: ( Has Logging.Capability eff
                             , Has Filesystem.Capability eff
                             )
                          => DocumentationDirectory
                          -> Effectful eff [Filesystem.Path]
getDocumentationFilenames (DocumentationDirectory docDir) = do
  Logging.debug ( "Reading documentation files at " <> docDir )
  getPathsWith (Extension ".md") <$> Filesystem.list (Path docDir)


getDocumentationDirectory :: Has Filesystem.Capability eff
                          => Effectful eff DocumentationDirectory
getDocumentationDirectory = do
  Filesystem.Path currentDir <- Filesystem.currentDirectory
  return ( DocumentationDirectory $ currentDir <> "/doc/" )



