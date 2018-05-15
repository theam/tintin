module Tintin
  ( runApp
  , OutputDirectory(..)
  , makeLogger
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
  filenames <- Filesystem.Path (currentDir <> "/doc")
               |>  Filesystem.list
               |$> Filesystem.getFilenamesWith (Filesystem.Extension ".md")

  -- FIXME
  ( errors, docFiles) <-  filenames
                          |>  mapM readAndParse
                          |$> map DocumentationFile.new
                          |$> partitionEithers

  unless (null errors) $
    errors
    |> map DocumentationFile.errorText
    |> mapM_ Logging.log
    |> const (error "Parse errors found. Exiting...")

  -- TODO: Parse docFiles and then, compile and render them
  putTextLn "FIX ME"

