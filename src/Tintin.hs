module Tintin
  ( runApp
  )
where

import Tintin.Core

import qualified Tintin.Capabilities.Logging as Logging
import qualified Tintin.Capabilities.Filesystem as Filesystem
import qualified Tintin.Domain.DocumentationFile as DocumentationFile


runApp :: ( Has Logging.Capability eff
          , Has Filesystem.Capability eff
          )
       => OutputDirectory
       -> Effectful eff ()
runApp (OutputDirectory outputDir) = do

  Logging.log "Cleaning output directory if it exists"
  Filesystem.deleteIfExists (Filesystem.Path outputDir)

  Logging.log "Reading documentation files"
  Filesystem.Path currentDir <- Filesystem.currentDirectory
  let DocumentationDirectory docs = DocumentationDirectory (currentDir <> "/doc")
  filenames <- Filesystem.list (Filesystem.Path docs)
               |$> Filesystem.getPathsWith (Filesystem.Extension ".md")

  -- FIXME
  ( errors, docFiles) <-  filenames
                          |>  mapM (readAndParse $ DocumentationDirectory docs)
                          |$> partitionEithers

  unless (null errors) (showErrorsAndDie errors)

  -- TODO: Parse docFiles and then, compile and render them
  putTextLn "FIX ME"


readAndParse :: ( Has Logging.Capability eff
                , Has Filesystem.Capability eff
                )
             => DocumentationDirectory
             -> Filesystem.Path
             -> Effectful eff (Either DocumentationFile.ParseError DocumentationFile.Value)
readAndParse ( DocumentationDirectory d ) ( Filesystem.Path f ) = do
  contents <- Filesystem.readFile ( Filesystem.Path $ d <> "/" <> f)
  return $ DocumentationFile.new (DocumentationFile.Filename f) contents


showErrorsAndDie :: Has Logging.Capability eff
                 => [DocumentationFile.ParseError]
                 -> Effectful eff ()
showErrorsAndDie errors = do
    errors
     |> map DocumentationFile.errorText
     |> mapM_ Logging.log
    error "Parse errors found. Exiting."


