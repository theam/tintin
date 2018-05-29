module Tintin.Render where

import Tintin.Core

require Tintin.Capabilities.Logging
require Tintin.Capabilities.Filesystem
require Tintin.Capabilities.Process
require Tintin.Domain.HtmlFile
require Tintin.Domain.DocumentationFile
require Tintin.Domain.Project
require Tintin.Html.Templating
require Tintin.Errors

require Data.Text

data Render

perform :: ( Has Logging.Capability eff
           , Has Filesystem.Capability eff
           , Has Process.Capability eff
           )
        => HtmlFile.BuildTool
        -> [DocumentationFile]
        -> Effectful eff [HtmlFile]
perform buildTool docFiles = do
  Logging.debug "Rendering"
  (errors, htmlFiles) <- docFiles
                         |>  map  HtmlFile.fromDocumentationFile
                         |>  mapM (HtmlFile.run buildTool)
                         |$> partitionEithers
  unless (null errors) (Errors.textDie (HtmlFile.showCompilationError <$> errors))
  return htmlFiles


writeOutput :: ( Has Logging.Capability eff
               , Has Filesystem.Capability eff
               )
            => OutputDirectory
            -> Project.Info
            -> Effectful eff ()
writeOutput (OutputDirectory od) info = do
  Filesystem.makeDirectory (Filesystem.Path od)
  Logging.debug "Writing HTML output"
  forM_ (Project.pages info) $ \page -> do
    let newContent = Templating.wrap info page
    let slash = if "/" `Text.isSuffixOf` od then "" else "/"
    Filesystem.writeFile (Filesystem.Path $ od <> slash <> Project.filename page) newContent


