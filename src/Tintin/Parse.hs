module Tintin.Parse
  ( docs )
where

import Tintin.Core
import qualified Tintin.Capabilities.Logging as Logging
import qualified Tintin.Capabilities.Filesystem as Filesystem
import qualified Tintin.Domain.DocumentationFile as DocumentationFile
import qualified Tintin.Errors as Errors


docs :: ( Has Logging.Capability eff
        , Has Filesystem.Capability eff
        )
     => DocumentationDirectory
     -> [Filesystem.Path]
     -> Effectful eff [DocumentationFile.Value]
docs docDir filenames = do
  Logging.debug "Parsing documentation"
  (errors, docFiles) <- filenames
                        |>  mapM (readAndParse docDir)
                        |$> partitionEithers
  unless (null errors) (Errors.showAndDie errors)
  return docFiles


readAndParse :: ( Has Logging.Capability eff
                , Has Filesystem.Capability eff
                )
             => DocumentationDirectory
             -> Filesystem.Path
             -> Effectful eff (Either DocumentationFile.ParseError DocumentationFile.Value)
readAndParse ( DocumentationDirectory d ) ( Filesystem.Path f ) = do
  contents <- Filesystem.readFile ( Filesystem.Path $ d <> "/" <> f)
  return $ DocumentationFile.new (DocumentationFile.Filename f) contents




