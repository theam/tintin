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

require Data.Map
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
  forM_ (withContext (Project.pages info)) $ \(page, context) -> do
    let newContent = Templating.wrap info context page
    let slash = if "/" `Text.isSuffixOf` od then "" else "/"
    Filesystem.writeFile (Filesystem.Path $ od <> slash <> Project.filename page) newContent

withContext
    :: [Project.Page]
    -> [(Project.Page, Project.Context)]
withContext ps = toList contextMap
  where
    indexRef :: Maybe Project.PageRef
    pageMap  :: Map Text Project.Page
    (First indexRef, pageMap)
        = foldMap (\p -> let fn = Project.filename p
                         in  if fn == "index.html"
                               then (First (Just (makeRef p)), mempty     )
                               else (mempty                  , one (fn, p))
                  ) ps
    contextMap :: Map Text (Project.Page, Project.Context)
    contextMap = foldMap (\p -> one (Project.filename p, (p, makeContext p))
                         ) ps
    makeContext :: Project.Page -> Project.Context
    makeContext p
        | fn == "index.html" = Map.lookupMin pageMap
                                 |$> snd
                                 |$> makeRef
                                 |> Project.Context Nothing
        | otherwise          =
            let prev = Map.lookupLT fn pageMap
                        |$> snd
                        |$> makeRef
                next = Map.lookupGT fn pageMap
                        |$> snd
                        |$> makeRef
            in  Project.Context (prev <|> indexRef) next  -- if no prev, use index
      where
        fn = Project.filename p
    makeRef :: Project.Page -> Project.PageRef
    makeRef p = Project.PageRef
      { refTitle    = Project.title p
      , refFilename = Project.filename p
      }
