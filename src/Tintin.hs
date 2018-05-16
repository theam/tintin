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
runApp outputDirectory = do
  cleanUp outputDirectory
  filenames <- getDocumentationFilenames
  docFiles  <- parseDocs filenames

  -- TODO: Compile and render docFiles
  putTextLn "FIX ME"



cleanUp :: ( Has Logging.Capability eff
           , Has Filesystem.Capability eff
           )
        => OutputDirectory
        -> Effectful eff ()
cleanUp (OutputDirectory p) = do
  Logging.log "Cleaning output directory"
  Filesystem.deleteIfExists (Filesystem.Path p)



getDocumentationFilenames :: ( Has Logging.Capability eff
                             , Has Filesystem.Capability eff
                             )
                          => Effectful eff [Filesystem.Path]
getDocumentationFilenames = do
  Logging.log "Reading documentation files"
  DocumentationDirectory d <- getDocumentationDirectory
  Filesystem.Path d
   |>  Filesystem.list
   |$> Filesystem.getPathsWith (Filesystem.Extension ".md")



getDocumentationDirectory :: Has Filesystem.Capability eff
                          => Effectful eff DocumentationDirectory
getDocumentationDirectory = do
  Filesystem.Path currentDir <- Filesystem.currentDirectory
  return ( DocumentationDirectory $ currentDir <> "/doc" )



parseDocs :: ( Has Logging.Capability eff
             , Has Filesystem.Capability eff
             )
          => [Filesystem.Path]
          -> Effectful eff [DocumentationFile.Value]
parseDocs filenames = do
  docDir <- getDocumentationDirectory
  (errors, docFiles) <- filenames
                        |>  mapM (readAndParse docDir)
                        |$> partitionEithers
  unless (null errors) (showErrorsAndDie errors)
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



showErrorsAndDie :: Has Logging.Capability eff
                 => [DocumentationFile.ParseError]
                 -> Effectful eff ()
showErrorsAndDie errors = do
    errors
     |> map DocumentationFile.errorText
     |> mapM_ Logging.log
    error "Parse errors found. Exiting."


