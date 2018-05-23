module Tintin.Parse
  ( Parse
  , docs
  )
where

import  Tintin.Core
require Tintin.Capabilities.Logging
require Tintin.Capabilities.Filesystem
require Tintin.Domain.DocumentationFile
require Tintin.Errors


data Parse

docs :: ( Has Logging.Capability eff
        , Has Filesystem.Capability eff
        )
     => DocumentationDirectory
     -> [Filesystem.Path]
     -> Effectful eff [DocumentationFile]
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
             -> Effectful eff (Either DocumentationFile.ParseError DocumentationFile)
readAndParse ( DocumentationDirectory d ) ( Filesystem.Path f ) = do
  contents <- Filesystem.readFile ( Filesystem.Path $ d <> "/" <> f)
  return $ DocumentationFile.new (DocumentationFile.Filename f) contents




