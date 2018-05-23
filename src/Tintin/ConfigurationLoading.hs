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
import Universum.Unsafe (fromJust)
import Text.Read (read)


data ConfigurationLoading

loadInfo :: ( Has Logging.Capability eff
            , Has Filesystem.Capability eff
            )
         => [HtmlFile]
         -> Effectful eff Project.Info
loadInfo htmlFiles = do
  let pages = htmlFiles
              |> map (\HtmlFile.HtmlFile {..} -> Project.Page title content filename)
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
        projectName     = projectInfoFile |> getFieldValue "name"
        projectSynopsis = projectInfoFile |> getFieldValue "synopsis"
        projectGithub   = (projectInfoFile |> getFieldValue "github")
                          <|> (projectInfoFile |> getFieldValue "location")
        projectAuthor   = projectGithub |$> getAuthor
        tintinColor     = tintinFile |> getFieldValue "color"
        tintinLogo      = tintinFile |> getFieldValue "logo"
      when (isNothing projectName) (Errors.showAndDie ["Project must have a name. Please set it in package.yaml or *.cabal."])
      when (isNothing projectSynopsis) (Errors.showAndDie ["Project must have a synopsis. Please set it in package.yaml or *.cabal."])
      when (isNothing projectGithub) (Errors.showAndDie ["Project must be hosted in a Github repository. Please set it in package.yaml or *.cabal."])
      when (isNothing tintinColor)
        (Errors.showAndDie ["Tintin usually generates a .tintin.yml file with a color configuration. Maybe you don't have enough permissions?\
                           \\n\nTry creating .tintin.yml and adding color:blue to it."])
      return Project.Info
        { name = fromJust projectName
        , synopsis = fromJust projectSynopsis
        , githubLink = parseGithubUrl $ fromJust projectGithub
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
                          |> lines
                          |> filter (\t -> field `Text.isPrefixOf` Text.strip t)
                          |> safeHead
                          |$> Text.strip
                          |>> Text.stripPrefix (field <> ":")
                          |$> Text.strip
  getAuthor txt =
    txt
    |> (Text.stripPrefix "\"" >=> Text.stripSuffix "\"")
    |> fromMaybe txt
    |> Text.takeWhile (/= '/')

  parseGithubUrl txt =
    txt
    |>  Text.stripPrefix "\""
    |>> Text.stripSuffix "\""
    |>  fromMaybe txt



