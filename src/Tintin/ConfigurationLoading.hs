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

data ProjectContext = ProjectContext
  { name            :: Maybe Text
  , synopsis        :: Maybe Text
  , github          :: Maybe Text
  , location        :: Maybe Text
  , author          :: Maybe Text
  , projectLocation :: Filesystem.Path
  }

getFieldValue :: Text -> Text -> Maybe Text
getFieldValue field txt = txt
                        & lines
                        & filter (\t -> field `Text.isPrefixOf` Text.strip t)
                        & safeHead
                        & fmap Text.strip
                        & flatMap (Text.stripPrefix $ field <> ":")
                        & fmap Text.strip

loadProjectConfig :: ( Has Logging.Capability eff
                     , Has Filesystem.Capability eff
                     )
                   => Filesystem.Path -> Effectful eff ProjectContext
loadProjectConfig currentPath = do
  files <- Filesystem.list currentPath
  let packageYamlFile = find isPackageYaml files
  let cabalFile       = find isCabalFile files
  case packageYamlFile <|> cabalFile of
    Just file -> do
      Logging.debug "Reading project info"
      projectConfigFile <- Filesystem.readFile file
      return ProjectContext
        { name            = projectConfigFile & getFieldValue "name"
        , synopsis        = projectConfigFile & getFieldValue "synopsis"
        , github          = projectConfigFile & getFieldValue "github"
        , location        = projectConfigFile & getFieldValue "location"
        , author          = projectConfigFile & getFieldValue "author"
        , projectLocation = currentPath
        }
    Nothing -> return ProjectContext
      { name            = Nothing
      , synopsis        = Nothing
      , github          = Nothing
      , location        = Nothing
      , author          = Nothing
      , projectLocation = currentPath
      }
 where
  isPackageYaml (Filesystem.Path p) = p == "package.yaml"
  isCabalFile   (Filesystem.Path p) = ".cabal" `Text.isInfixOf` p

loadTintinConfig :: ( Has Logging.Capability eff
                    , Has Filesystem.Capability eff
                    )
                 => [Project.Page]
                 -> ProjectContext
                 -> Effectful eff Project.Info
loadTintinConfig pages ProjectContext {..} = do
  let Filesystem.Path currentPath = projectLocation
      tintinPath = Filesystem.Path $ currentPath <> "/.tintin.yml"
  tintinExists <- Filesystem.doesExist tintinPath
  unless tintinExists $ Filesystem.writeFile tintinPath "color: blue\n"
  tintinConfig <- Filesystem.readFile tintinPath
  let
    projectName           = (tintinConfig & getFieldValue "name") <|> name
    projectSynopsis       = (tintinConfig & getFieldValue "synopsis") <|> synopsis
    projectGithub         = (tintinConfig & getFieldValue "github") <|> github <|> location
    projectAuthor         = (tintinConfig & getFieldValue "author") <|> author
    tintinColor           = tintinConfig & getFieldValue "color"
    tintinLogo            = tintinConfig & getFieldValue "logo"
    tintinTitleFont       = (tintinConfig & getFieldValue "titleFont") <|> (Just $ fromString "Montserrat")
    tintinTitleFontWeight = (tintinConfig & getFieldValue "titleFontWeight") <|> (Just "500")
    tintinBodyFont        = (tintinConfig & getFieldValue "bodyFont") <|> (Just $ fromString "IBM+Plex+Sans")
  when (isNothing projectName) (Errors.showAndDie ["Project must have a name. Please set it in package.yaml or *.cabal."])
  when (isNothing projectSynopsis) (Errors.showAndDie ["Project must have a synopsis. Please set it in package.yaml or *.cabal."])
  when (isNothing tintinColor) (Errors.showAndDie [errorMessages])
  return Project.Info
    { name = Unsafe.fromJust projectName
    , synopsis        = Unsafe.fromJust projectSynopsis
    , githubLink      = parseGithubUrl <$> projectGithub
    , githubAuthor    = projectAuthor
    , color           = makeColor $ Unsafe.fromJust tintinColor
    , logoUrl         = tintinLogo
    , titleFont       = Unsafe.fromJust tintinTitleFont
    , titleFontWeight = read $ toString $ Unsafe.fromJust tintinTitleFontWeight
    , bodyFont        = Unsafe.fromJust tintinBodyFont
    , pages           = pages
    }
 where
  parseGithubUrl txt =
    let unquoted = stripGit $ stripQuotes txt
    in unquoted
       & Text.stripPrefix "github.com/"
       & fromMaybe unquoted
  stripGit txt =
    txt
      & Text.stripPrefix "git://"
      & flatMap (Text.stripSuffix ".git")
      & fromMaybe txt
  stripQuotes txt =
    txt
    & Text.stripPrefix "\""
    & flatMap (Text.stripSuffix "\"")
    & fromMaybe txt
  errorMessages = unlines
    ["Tintin usually generates a .tintin.yml file with a color configuration. Maybe you don't have enough permissions?"
    , ""
    , ""
    , "Try creating .tintin.yml and adding color:blue to it."
    ]

makeColor :: Text -> Project.Color
makeColor txt =
  case Text.stripPrefix "#" txt of
    Just _ -> Project.HexColor txt
    Nothing -> Inflections.SomeWord <$> Inflections.mkWord txt
      & Inflections.titleize
      & toString
      & read

loadInfo :: ( Has Logging.Capability eff
            , Has Filesystem.Capability eff
            )
         => [HtmlFile]
         -> Effectful eff Project.Info
loadInfo htmlFiles =
  Filesystem.currentDirectory
    >>= loadProjectConfig
    >>= loadTintinConfig pages
 where
  pages = htmlFiles & map (\HtmlFile.HtmlFile {..} -> Project.Page title content filename)
