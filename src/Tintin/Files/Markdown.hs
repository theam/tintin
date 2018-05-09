module Tintin.Files.Markdown
  ( getFrom )
where

import Tintin.Core

import Data.Text (isSuffixOf)


getFrom :: DocumentationDirectory -> IO MarkdownFiles
getFrom ( DocumentationDirectory path ) = do
  files <- listDirectory $ toString path
  let markdownFiles = filter (isSuffixOf ".md") $ toText <$> files
  return MarkdownFiles
    { markdownFilesList = markdownFiles
    , markdownFilesDirectory = path
    }

