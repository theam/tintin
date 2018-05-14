module Tintin.Domain where

import Universum

import Data.Yaml


newtype DocumentationDirectory = DocumentationDirectory Text
newtype OutputDirectory        = OutputDirectory Text
newtype TemporaryDirectory     = TemporaryDirectory Text


data MarkdownFiles = MarkdownFiles
  { markdownFilesList      :: [Text]
  , markdownFilesDirectory :: Text
  }

data HaskellFiles = HaskellFiles
  { haskellFilesData       :: [HaskellFile]
  , haskellFilesDirectory  :: Text
  }

data HaskellFile = HaskellFile
  { haskellFileFrontMatter :: Text
  , haskellFileContent     :: Text
  , haskellFileName        :: Text
  }

data RenderedData = RenderedData
  { renderedDataContent    :: Text
  , renderedDataFile       :: Text
  , renderedDataTitle      :: Text
  }

data FrontMatter = FrontMatter
  { frontMatterTitle :: Text
  , frontMatterSubitle :: Maybe Text
  , frontMatterLayout :: Maybe Text
  }


instance FromJSON FrontMatter where
  parseJSON (Object v) =
    FrontMatter
    <$> v .: "title"
    <*> v .:? "subtitle"
    <*> v .:? "layout"
  parseJSON _ = fail "Expected object"

