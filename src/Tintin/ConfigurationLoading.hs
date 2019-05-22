module Tintin.ConfigurationLoading
  ( ConfigurationLoading
  , loadInfo
  )
where

require Tintin.Errors
require Tintin.Capabilities.Logging
require Tintin.Capabilities.Filesystem
require Tintin.Domain.HtmlFile
require Tintin.Domain.Project

require Data.Text

import Tintin.Core
import qualified Universum.Unsafe as Unsafe
import Text.Read (read)
import qualified Text.Inflections as Inflections


data ConfigurationLoading

loadInfo :: ( Has Logging.Capability eff
            , Has Filesystem.Capability eff
            )
         => [HtmlFile]
         -> Effectful eff Project.Info
loadInfo htmlFiles = do
  let pages = htmlFiles
              & map (\HtmlFile.HtmlFile {..} -> Project.Page title content filename)
  Filesystem.Path currentDir <- Filesystem.currentDirectory
  files <- Filesystem.list (Filesystem.Path currentDir)
  let packageYamlFile = find isPackageYaml files
  let cabalFile       = find isCabalFile files
  case packageYamlFile <|> cabalFile of
    Nothing -> do
      Errors.showAndDie ["No package.yaml or *.cabal file found."]
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
        projectName     = projectInfoFile & getFieldValue "name"
        projectSynopsis = projectInfoFile & getFieldValue "synopsis"
        projectGithub   = (projectInfoFile & getFieldValue "github")
                          <|> (projectInfoFile & getFieldValue "location")
        projectAuthor   = projectGithub & fmap getAuthor
        tintinColor     = tintinFile & getFieldValue "color"
        tintinLogo      = tintinFile & getFieldValue "logo"
      when (isNothing projectName) (Errors.showAndDie ["Project must have a name. Please set it in package.yaml or *.cabal."])
      when (isNothing projectSynopsis) (Errors.showAndDie ["Project must have a synopsis. Please set it in package.yaml or *.cabal."])
      when (isNothing tintinColor)
        (Errors.showAndDie [errorMessages])
      return Project.Info
        { name = Unsafe.fromJust projectName
        , synopsis = Unsafe.fromJust projectSynopsis
        , githubLink = parseGithubUrl <$> projectGithub
        , githubAuthor = projectAuthor
        , color = makeColor $ Unsafe.fromJust tintinColor
        , logoUrl = tintinLogo
        , pages = pages
        }

 where
  errorMessages = unlines
    ["Tintin usually generates a .tintin.yml file with a color configuration. Maybe you don't have enough permissions?"
    , ""
    , ""
    , "Try creating .tintin.yml and adding color:blue to it."
    ]
  isPackageYaml (Filesystem.Path p) =
    p == "package.yaml"

  isCabalFile   (Filesystem.Path p) =
    ".cabal" `Text.isInfixOf` p

  makeColor :: Text -> Project.Color
  makeColor txt =
    case Text.stripPrefix "#" txt of
      Just _ -> Project.HexColor txt
      Nothing -> Inflections.SomeWord <$> Inflections.mkWord txt
        & Inflections.titleize 
        & toString
        & read

  getFieldValue field txt = txt
                          & lines
                          & filter (\t -> field `Text.isPrefixOf` Text.strip t)
                          & safeHead
                          & fmap Text.strip
                          & flatMap (Text.stripPrefix $ field <> ":")
                          & fmap Text.strip
  getAuthor txt =
    let unquoted = stripQuotes txt
    in parseGithubUrl unquoted
       & (\t -> if "http" `Text.isPrefixOf` t
                then Text.splitOn "/" t
                     & filter (not . Text.isInfixOf "git")
                     & filter (not . Text.null)
                     & Unsafe.tail
                     & Unsafe.head
                else t
         )
       & Text.takeWhile (/= '/')

  parseGithubUrl txt =
    let unquoted = stripGit $ stripQuotes txt
    in unquoted
       & Text.stripPrefix "github.com/"
       & fromMaybe unquoted

  stripQuotes txt =
    txt
    & Text.stripPrefix "\""
    & flatMap (Text.stripSuffix "\"")
    & fromMaybe txt

  stripGit txt =
    txt
    & Text.stripPrefix "git://"
    & flatMap (Text.stripSuffix ".git")
    & fromMaybe txt


