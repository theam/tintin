module Tintin.Files.Html
  ( render
  , write
  )
where


import Data.Text (toLower, isSuffixOf)
import Data.Yaml

import Tintin.Core
import Tintin.Html.Templating as Template


render :: HaskellFiles -> IO [RenderedData]
render HaskellFiles {..} = forM haskellFilesData $ \HaskellFile {..} -> do
  let filename = toString $ haskellFilesDirectory <> "/" <> haskellFileName
  writeHaskellFile filename haskellFileContent
  rendered <- readProcess "stack" ["runghc", filename, "--", "--no-inlit-wrap"] ""
  let htmlFileName = haskellFileName `changeExtensionTo` ".html"
  case decode ( encodeUtf8 @Text @ByteString haskellFileFrontMatter ) of
    Nothing -> error "Parse error"
    Just fm ->
      return RenderedData
        { renderedDataContent = toText rendered
        , renderedDataFile = toLower htmlFileName
        , renderedDataTitle = frontMatterTitle fm
        }
 where
  writeHaskellFile filename source =
    writeFile filename ( "{-# OPTIONS_GHC -F -pgmF inlitpp #-}\n" <> source )


write :: OutputDirectory -> [RenderedData] -> IO ()
write (OutputDirectory outputDir) renderedData =
  forM_ renderedData $ \rd@RenderedData {..} -> do
    let outputDirectory = outputDir <> "/pages"
    let filename        = toString $ outputDirectory <> "/" <> renderedDataFile
    createDirectoryIfMissing True ( toString outputDirectory )
    let wrappedContent =
          if "index.html" `isSuffixOf` renderedDataFile
          then Template.wrapHome renderedData rd
          else Template.wrapPage renderedData rd
    writeFile filename wrappedContent

