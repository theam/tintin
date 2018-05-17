module Tintin.Render where

import Tintin.Core
import Tintin.Capabilities.Logging as Logging
import Tintin.Capabilities.Filesystem as Filesystem
import Tintin.Capabilities.Process as Process
import Tintin.Domain.HtmlFile as HtmlFile
import Tintin.Domain.DocumentationFile as DocumentationFile
import Tintin.Domain.Project as Project
import Tintin.Html.Templating as Templating
import Tintin.Errors as Errors

perform :: ( Has Logging.Capability eff
           , Has Filesystem.Capability eff
           , Has Process.Capability eff
           )
        => [DocumentationFile.Value]
        -> Effectful eff [HtmlFile.Value]
perform docFiles = do
  Logging.debug "Rendering"
  (errors, htmlFiles) <- docFiles
                         |>  map  HtmlFile.fromDocumentationFile
                         |>  mapM HtmlFile.run
                         |$> partitionEithers
  unless (null errors) (Errors.showAndDie errors)
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
    Filesystem.writeFile (Filesystem.Path $ od <> Project.filename page) newContent


