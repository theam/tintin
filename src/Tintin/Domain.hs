module Tintin.Domain where

import Universum


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
