module Tintin
  ( runApp
  )
where

import Tintin.Core

require Tintin.Capabilities.Logging
require Tintin.Capabilities.Filesystem
require Tintin.Capabilities.Process
require Tintin.Parse
require Tintin.Render
require Tintin.ConfigurationLoading
require Tintin.Domain.HtmlFile


runApp :: ( Has Logging.Capability eff
          , Has Filesystem.Capability eff
          , Has Process.Capability eff
          )
       => Bool
       -> OutputDirectory
       -> Effectful eff ()
runApp shouldUseCabal outputDirectory = do
  cleanUp outputDirectory
  docDir    <- getDocumentationDirectory
  filenames <- getDocumentationFilenames docDir
  let buildTool = if shouldUseCabal then HtmlFile.Cabal else HtmlFile.Stack

  Parse.docs docDir filenames
   |>> Render.perform buildTool
   |>> ConfigurationLoading.loadInfo
   |>> Render.writeOutput outputDirectory


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
  Filesystem.Path docDir
   |>  Filesystem.list
   |$> Filesystem.getPathsWith (Filesystem.Extension ".md")



getDocumentationDirectory :: Has Filesystem.Capability eff
                          => Effectful eff DocumentationDirectory
getDocumentationDirectory = do
  Filesystem.Path currentDir <- Filesystem.currentDirectory
  return ( DocumentationDirectory $ currentDir <> "/doc/" )



