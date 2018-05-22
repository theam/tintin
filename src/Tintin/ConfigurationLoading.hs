module Tintin.ConfigurationLoading
  ( loadInfo )
where

import Tintin.Core
import qualified Tintin.Errors as E (showAndDie)
import qualified Tintin.Capabilities.Logging as L (Capability(..), debug)
-- import qualified Tintin.Capabilities.Filesystem as Filesystem
import qualified Tintin.Capabilities.Filesystem as FS (Path(..), Capability(..), currentDirectory, readFile, writeFile, doesExist, list)
import qualified Tintin.Domain.HtmlFile as HtmlFile
import qualified Tintin.Domain.Project as Project

import qualified Data.Text as T (head, tail, singleton, toUpper, isPrefixOf, strip, stripPrefix, stripSuffix, takeWhile, isInfixOf)
import Universum.Unsafe (fromJust)
import Text.Read (read)

import qualified Universum.Debug as Debug

-- loadInfo :: ( Has L.Capability eff
--             , Has FS.Capability eff
--             )
--          => [HtmlFile.Value]
--          -> Effectful eff Project.Info
loadInfo htmlFiles = do
  let pages = htmlFiles
              |> map (\HtmlFile.Value {..} -> Project.Page title content filename)
  FS.Path currentDir <- FS.currentDirectory
  files <- FS.list (FS.Path currentDir)
  let packageYamlFile = find isPackageYaml files
  let cabalFile       = find isCabalFile files
  case packageYamlFile <|> cabalFile of
    Nothing -> do
      E.showAndDie ["No package.yaml or *.cabal file found."]
      error ""

    Just p -> do
      let tintinPath = FS.Path $ currentDir <> "/.tintin.yml"
      L.debug "Reading project info"
      projectInfoFile <- FS.readFile p
      tintinExists    <- FS.doesExist tintinPath
      unless tintinExists $
        FS.writeFile tintinPath "color: blue\n"
      tintinFile <- FS.readFile tintinPath
      let
        projectName     = projectInfoFile |> getFieldValue "name"
        projectSynopsis = projectInfoFile |> getFieldValue "synopsis"
        projectGithub   = (projectInfoFile |> getFieldValue "github")
                          <|> (projectInfoFile |> getFieldValue "location")
        projectAuthor   = projectGithub |$> getAuthor
        tintinColor     = tintinFile |> getFieldValue "color"
        tintinLogo      = tintinFile |> getFieldValue "logo"
      when (isNothing projectName) (E.showAndDie ["Project must have a name. Please set it in package.yaml or *.cabal."])
      when (isNothing projectSynopsis) (E.showAndDie ["Project must have a synopsis. Please set it in package.yaml or *.cabal."])
      when (isNothing projectGithub) (E.showAndDie ["Project must be hosted in a Github repository. Please set it in package.yaml or *.cabal."])
      when (isNothing tintinColor)
        (E.showAndDie ["Tintin usually generates a .tintin.yml file with a color configuration. Maybe you don't have enough permissions?\
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
  isPackageYaml (FS.Path p) =
    p == "package.yaml"

  isCabalFile   (FS.Path p) =
    ".cabal" `T.isInfixOf` p

  makeColor :: Text -> Project.Color
  makeColor txt =
    let capitalLetter = txt
                        |> T.head
                        |> T.singleton
                        |> T.toUpper
        restOfText    = txt
                        |> T.tail
    in  (capitalLetter <> restOfText)
         |> toString
         |> read

  getFieldValue field txt = txt
                          |> lines
                          |> filter (\t -> field `T.isPrefixOf` T.strip t)
                          |> safeHead
                          |$> T.strip
                          >>= T.stripPrefix (field <> ":")
                          |$> T.strip
  getAuthor txt =
    txt
    |> (T.stripPrefix "\"" >=> T.stripSuffix "\"")
    |> fromMaybe txt
    |> T.takeWhile (/= '/')

  parseGithubUrl txt =
    txt
    |>  T.stripPrefix "\""
    >>= T.stripSuffix "\""
    |>  fromMaybe txt



