module Tintin.ConfigurationLoading
  ( loadInfo )
where

import Tintin.Core
import qualified Tintin.Errors as E (showAndDie)
import qualified Tintin.Capabilities.Logging as Logging 
import Tintin.Capabilities.Logging (debug)
import qualified Tintin.Capabilities.Filesystem as FileSystem
import qualified Tintin.Capabilities.Filesystem as FS (Path(..), readFile, writeFile, doesExist, currentDirectory, list)
import qualified Tintin.Domain.HtmlFile as HtmlFile
import qualified Tintin.Domain.Project as Project

import qualified Data.Text as T (head, tail, singleton, toUpper, isPrefixOf, strip, stripPrefix, stripSuffix, takeWhile, isInfixOf)
import Universum.Unsafe (fromJust)
import Text.Read (read)

import qualified Universum.Debug as Debug

loadInfo :: ( Has Logging.Capability eff
            , Has FileSystem.Capability eff
            )
         => [HtmlFile.Value]
         -> Effectful eff Project.Info
loadInfo htmlFiles = do
  let pages = map (\HtmlFile.Value {..} -> Project.Page title content filename) htmlFiles
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
      debug "Reading project info"
      projectInfoFile <- FS.readFile p
      tintinExists    <- FS.doesExist tintinPath
      unless tintinExists $
        FS.writeFile tintinPath "color: blue\n"
      tintinFile <- FS.readFile tintinPath
      let
        projectName     = getFieldValue "name" projectInfoFile
        projectSynopsis = getFieldValue "synopsis" projectInfoFile
        projectGithub   = getFieldValue "github" projectInfoFile
                          <|> getFieldValue "location" projectInfoFile
        projectAuthor   = getAuthor <$> projectGithub 
        tintinColor     = getFieldValue "color" tintinFile
        tintinLogo      = getFieldValue "logo" tintinFile
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
    let capitalLetter = T.toUpper $ T.singleton $ T.head txt
        restOfText    = T.tail txt
    in  read $ toString (capitalLetter <> restOfText)

getFieldValue :: Text -> Text -> Maybe Text
getFieldValue field txt = do
  mt0 <- safeHead $ filter (\t -> field `T.isPrefixOf` T.strip t) $ lines txt
  mt1 <- T.stripPrefix (field <> ":") $ T.strip mt0
  pure $ T.strip mt1
  
getAuthor :: Text -> Text
getAuthor txt = T.takeWhile (/= '/') $
  fromMaybe txt $ (T.stripPrefix "\"" >=> T.stripSuffix "\"") txt

parseGithubUrl :: Text -> Text
parseGithubUrl txt = fromMaybe txt $ 
  T.stripPrefix "\"" txt >>= T.stripSuffix "\""



