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
                         &  map  HtmlFile.fromDocumentationFile
                         &  mapM (HtmlFile.run buildTool)
                         & fmap partitionEithers
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

-- | Pair up each page with context (next and previous links) by
-- alphabetical order, excluding index page.
--
-- This function is a little more complicated than might be expected
-- because the functionality is a little nuanced.  It has to:
--
-- 1.  Pair each page with next and previous links based on an alphabetical
--     sorting, /excluding/ the index page.
-- 2.  Give the /first/ non-index page, alphabetically, a "previous" link
--     pointing to the index /if it exists/.
-- 3.  Give the index, if it exists, a "next' link to the first non-index
--     page.
--
withContext
    :: [Project.Page]
    -> [(Project.Page, Project.Context)]
withContext ps = elems contextMap
  where
    -- | Pre-processing function.  Using a single fold, pick out the index
    -- page link (if it exists) and also accumulate a Map of all non-index
    -- pages, keyed by filenames.  The map enforces that the filenames are
    -- stored alphabetically.
    indexRef :: Maybe Project.PageRef
    pageMap  :: Map Text Project.Page
    (First indexRef, pageMap)
        = foldMap (\p -> let fn = Project.filename p
                         in  if fn == "index.html"
                               then (First (Just (makeRef p)), mempty     )
                               else (mempty                  , one (fn, p))
                  ) ps
    -- | Accumulate a final result map pairing each file with its
    -- 'makeContext' result.  The keys are the filenames, to enforce
    -- alphabetical sorting.
    contextMap :: Map Text (Project.Page, Project.Context)
    contextMap = ps
             & fmap (\p -> (Project.filename p, (p, makeContext p)))
             & Map.fromList
    -- | Actual function that pairs up each page with its context (next and
    -- previous links).
    --
    -- If the page is "index.html", use just the first item in 'pageMap',
    -- the map of non-index pages, using 'Map.lookupMin'
    --
    -- Otherwise, use 'Map.lookupLT' and 'Map.lookupGT' to find the "next
    -- and previous" pages in 'pageMap' (the map of non-index pages).  And,
    -- if there is no previous page, use the index page instead.
    makeContext :: Project.Page -> Project.Context
    makeContext p
        | fn == "index.html" = Map.lookupMin pageMap
                                 & fmap snd
                                 & fmap makeRef
                                 & Project.Context Nothing
        | otherwise          =
            let prev = Map.lookupLT fn pageMap
                        & fmap snd
                        & fmap makeRef
                next = Map.lookupGT fn pageMap
                        & fmap snd
                        & fmap makeRef
            in  Project.Context (prev <|> indexRef) next  -- if no prev, use index
      where
        fn = Project.filename p
    -- | Construct a link/reference from a page.
    makeRef :: Project.Page -> Project.PageRef
    makeRef p = Project.PageRef
      { refTitle    = Project.title p
      , refFilename = Project.filename p
      }
