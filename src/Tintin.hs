module Tintin
  ( runApp
  , OutputDirectory(..)
  , makeLogger
  )
where

import Tintin.Core
import Tintin.Files.Markdown as MarkdownFiles
import Tintin.Files.Haskell as HaskellFiles
import Tintin.Files.Html as HtmlFiles


runApp :: Logger -> OutputDirectory -> IO ()
runApp doLog (OutputDirectory outputDir) = withSystemTempDirectory "tintin" $ \tmpdir -> do
  currentDir <- toText <$> getCurrentDirectory

  doLog "Deleting output directory if it exists"
  deleteOutputDirectoryIfExists (OutputDirectory outputDir)

  doLog "Parsing Markdown files"
  markdownFiles <- MarkdownFiles.getFrom (DocumentationDirectory $ currentDir <> "/doc/")

  doLog "Converting to Haskell files"
  haskellFiles  <- HaskellFiles.convertMarkdown (TemporaryDirectory $ toText tmpdir) markdownFiles

  doLog "Compiling and rendering"
  renderedHtmlFiles <- HtmlFiles.render haskellFiles

  doLog "Writing to output"
  HtmlFiles.write (OutputDirectory outputDir) renderedHtmlFiles

  doLog ( "Website generated at " <> outputDir )

