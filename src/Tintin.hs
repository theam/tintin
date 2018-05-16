module Tintin
  ( runApp
  )
where

import Tintin.Core

import qualified Tintin.Capabilities.Logging as Logging
import qualified Tintin.Capabilities.Filesystem as Filesystem
import qualified Tintin.Capabilities.Process as Process
import qualified Tintin.Domain.DocumentationFile as DocumentationFile
import qualified Tintin.Domain.HtmlFile as HtmlFile
import qualified Tintin.Domain.Project as Project
import qualified Tintin.Html.Templating as Templating


runApp :: ( Has Logging.Capability eff
          , Has Filesystem.Capability eff
          , Has Process.Capability eff
          )
       => OutputDirectory
       -> Effectful eff ()
runApp outputDirectory = do
  cleanUp outputDirectory
  filenames <- getDocumentationFilenames
  docFiles  <- parseDocs filenames
  htmlFiles <- render docFiles
  writeOutput outputDirectory htmlFiles



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
                          => Effectful eff [Filesystem.Path]
getDocumentationFilenames = do
  DocumentationDirectory d <- getDocumentationDirectory
  Logging.debug ( "Reading documentation files at " <> d )
  Filesystem.Path d
   |>  Filesystem.list
   |$> Filesystem.getPathsWith (Filesystem.Extension ".md")



getDocumentationDirectory :: Has Filesystem.Capability eff
                          => Effectful eff DocumentationDirectory
getDocumentationDirectory = do
  Filesystem.Path currentDir <- Filesystem.currentDirectory
  return ( DocumentationDirectory $ currentDir <> "/doc/" )



parseDocs :: ( Has Logging.Capability eff
             , Has Filesystem.Capability eff
             )
          => [Filesystem.Path]
          -> Effectful eff [DocumentationFile.Value]
parseDocs filenames = do
  Logging.debug "Parsing documentation"
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



showErrorsAndDie :: ( Has Logging.Capability eff
                    , Show error
                    )
                 => [error]
                 -> Effectful eff ()
showErrorsAndDie errors = do
    errors
     |> mapM_ (Logging.err . show)
    error "Parse errors found. Exiting."


render :: ( Has Logging.Capability eff
          , Has Filesystem.Capability eff
          , Has Process.Capability eff
          )
       => [DocumentationFile.Value]
       -> Effectful eff [HtmlFile.Value]
render docFiles = do
  Logging.debug "Rendering"
  (errors, htmlFiles) <- docFiles
                         |>  map  HtmlFile.fromDocumentationFile
                         |>  mapM HtmlFile.run
                         |$> partitionEithers
  unless (null errors) (showErrorsAndDie errors)
  return htmlFiles


writeOutput :: ( Has Logging.Capability eff
               , Has Filesystem.Capability eff
               )
            => OutputDirectory
            -> [HtmlFile.Value]
            -> Effectful eff ()
writeOutput (OutputDirectory od) htmlFiles = do
  Logging.debug "Writing HTML output"
  Filesystem.makeDirectory (Filesystem.Path od)
  let pages = htmlFiles
              |> map (\HtmlFile.Value {..} -> Project.Page title content filename)
  let info = Project.Info
        { name = "tintin"
        , synopsis = "some testing"
        , color = Project.Blue
        , githubLink = "https://github.com/theam/tintin"
        , githubAuthor = "theam"
        , pages = pages
        , logoUrl = Just "../../../assets/logo.svg"
        }
  forM_ pages $ \page -> do
    let newContent = Templating.wrap info page
    Filesystem.writeFile (Filesystem.Path $ od <> Project.filename page) newContent

