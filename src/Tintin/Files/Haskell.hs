module Tintin.Files.Haskell
  ( convertMarkdown
  )
where

import Data.Text as Text (splitOn, isPrefixOf)
import qualified Data.List.NonEmpty as NonEmpty

import Tintin.Core


convertMarkdown :: TemporaryDirectory -> MarkdownFiles -> IO HaskellFiles
convertMarkdown ( TemporaryDirectory tmpdir ) MarkdownFiles {..} = do
  haskellFiles <- makeHaskellFiles
  return HaskellFiles
    { haskellFilesData = haskellFiles
    , haskellFilesDirectory = tmpdir
    }
 where
  makeHaskellFiles =
    forM markdownFilesList $ \markdownFile -> do
      content <- readFile ( toString $ markdownFilesDirectory <> markdownFile )
      unless ( hasFrontMatter content ) ( dieWithFrontMatterError markdownFile )
      let [ frontMatter, source ] = splitMarkdown content
      let filename = markdownFile `changeExtensionTo` ".hs"
      return HaskellFile
        { haskellFileFrontMatter = frontMatter
        , haskellFileContent     = source
        , haskellFileName        = filename
        }

  hasFrontMatter content =
    "---" `Text.isPrefixOf` content

  dieWithFrontMatterError filename =
    error $ "File " <> toText filename <> " must start with a front-matter."

  splitMarkdown =
    tail . NonEmpty.fromList . splitOn "---"



