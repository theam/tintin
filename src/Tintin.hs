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

import qualified Data.Text as Text
import Universum.Unsafe (fromJust)
import qualified Universum.Debug as Debug
import Text.Read (read)


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
    error "Errors found. Exiting."


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
  Filesystem.makeDirectory (Filesystem.Path od)
  let pages = htmlFiles
              |> map (\HtmlFile.Value {..} -> Project.Page title content filename)
  info <- readInfo pages
  Logging.debug "Writing HTML output"
  forM_ pages $ \page -> do
    let newContent = Templating.wrap info page
    Filesystem.writeFile (Filesystem.Path $ od <> Project.filename page) newContent


readInfo :: ( Has Logging.Capability eff
            , Has Filesystem.Capability eff
            )
         => [Project.Page]
         -> Effectful eff Project.Info
readInfo pages = do
  Filesystem.Path currentDir <- Filesystem.currentDirectory
  files <- Filesystem.list (Filesystem.Path currentDir)
  let packageYamlFile = find isPackageYaml files
  let cabalFile       = find isCabalFile files
  case packageYamlFile <|> cabalFile of
    Nothing -> do
      showErrorsAndDie ["No package.yaml or *.cabal file found."]
      error ""

    Just p -> do
      let tintinPath = Filesystem.Path $ currentDir <> "/.tintin.yml"
      Logging.debug "Reading project info"
      projectInfoFile <- Filesystem.readFile p
      tintinExists    <- Filesystem.doesExist tintinPath
      unless tintinExists $
        Filesystem.writeFile tintinPath "color: blue\n"
      tintinFile <- Filesystem.readFile tintinPath
      let
        projectName     = projectInfoFile |> getFieldValue "name"
        projectSynopsis = projectInfoFile |> getFieldValue "synopsis"
        projectGithub   = (projectInfoFile |> getFieldValue "github")
                          <|> (projectInfoFile |> getFieldValue "location")
        projectAuthor   = projectGithub |$> getAuthor
        tintinColor     = tintinFile |> getFieldValue "color"
        tintinLogo      = tintinFile |> getFieldValue "logo"
      when (isNothing projectName) (showErrorsAndDie ["Project must have a name. Please set it in package.yaml or *.cabal."])
      when (isNothing projectSynopsis) (showErrorsAndDie ["Project must have a synopsis. Please set it in package.yaml or *.cabal."])
      when (isNothing projectGithub) (showErrorsAndDie ["Project must be hosted in a Github repository. Please set it in package.yaml or *.cabal."])
      when (isNothing tintinColor)
        (showErrorsAndDie ["Tintin usually generates a .tintin.yml file with a color configuration. Maybe you don't have enough permissions?\
                           \\n\nTry creating .tintin.yml and adding color:blue to it."])
      return Project.Info
        { name = fromJust projectName
        , synopsis = fromJust projectSynopsis
        , githubLink = fromJust projectGithub
        , githubAuthor = fromJust projectAuthor
        , color = makeColor $ fromJust tintinColor
        , logoUrl = tintinLogo
        , pages = pages
        }

 where
  isPackageYaml (Filesystem.Path p) =
    p == "package.yaml"

  isCabalFile   (Filesystem.Path p) =
    ".cabal" `Text.isInfixOf` p

  makeColor :: Text -> Project.Color
  makeColor txt =
    let capitalLetter = txt
                        |> Text.head
                        |> Text.singleton
                        |> Text.toUpper
        restOfText    = txt
                        |> Text.tail
    in  (capitalLetter <> restOfText)
         |> toString
         |> read

  getFieldValue field txt = txt
                          |> Text.lines
                          |> filter (field `Text.isPrefixOf`)
                          |> safeHead
                          |>> Text.stripPrefix (field <> ":")
                          |$> Text.strip
  getAuthor txt = txt
                  |> Text.stripPrefix "https://github.com/"
                  |> fromMaybe txt
                  |> Text.stripPrefix "\""
                  |> fromMaybe txt
                  |> Text.takeWhile (/= '/')

