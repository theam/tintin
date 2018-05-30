module Tintin
  ( runApp
  , publish
  )
where

import Tintin.Core

require Tintin.Capabilities.Logging
require Tintin.Capabilities.Filesystem
require Tintin.Capabilities.Process
require Tintin.Parse
require Tintin.Render
require Tintin.Errors
require Tintin.ConfigurationLoading
require Tintin.Domain.HtmlFile

require Data.Text



publish :: ( Has Logging.Capability eff
           , Has Filesystem.Capability eff
           , Has Process.Capability eff
           )
        => OutputDirectory
        -> Effectful eff ()
publish (OutputDirectory p)= do
  gitContents <- Filesystem.readFile ( Filesystem.Path ".git/config" )
  let r = lines gitContents
          |>  dropWhile (not . Text.isInfixOf "origin")
          |>  nonEmpty
          |$> tail
          |>> safeHead
          |$> Text.dropWhile (/= '=')
          |$> Text.dropWhile (/= 'g')
  case r of
    Nothing ->
      Errors.textDie ["Could not read origin remote. Are you in a Git repository?"]

    Just remote -> do
      Logging.debug "Initializing repo"
      Process.call ( Process.CommandName $ "cd " <> p
                     <> " && git init"
                   )
      Logging.debug "Adding origin remote"
      Process.call ( Process.CommandName $ "cd " <> p
                     <> " && git remote add origin " <> remote
                   )
      Logging.debug "Cheking out gh-pages"
      Process.call ( Process.CommandName $ "cd " <> p
                     <> " && git checkout -b gh-pages"
                   )
      Logging.debug "Adding new docs"
      Process.call ( Process.CommandName $ "cd " <> p
                     <> " && git add *"
                   )
      Logging.debug "Commiting"
      Process.call ( Process.CommandName $ "cd " <> p
                     <> " && git commit -m 'Update docs'"
                   )
      Logging.debug "Pushing"
      Process.call ( Process.CommandName $ "cd " <> p
                     <> " && git push -f origin gh-pages"
                   )


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



