module Tintin.Files.Html
  ( render
  , write
  )
where


import Data.Text (toLower)

import Tintin.Core


render :: HaskellFiles -> IO [RenderedData]
render HaskellFiles {..} = forM haskellFilesData $ \HaskellFile {..} -> do
  let filename = toString $ haskellFilesDirectory <> "/" <> haskellFileName
  writeHaskellFile filename haskellFileContent
  rendered <- readProcess "stack" ["runghc", filename, "--", "--no-inlit-wrap"] ""
  let htmlFileName = haskellFileName `changeExtensionTo` ".html"
  let header = "---" <> haskellFileFrontMatter <> "layout: page\n---\n"
  return RenderedData
    { renderedDataContent = header <> toText rendered
    , renderedDataFile = toLower htmlFileName
    }
 where
  writeHaskellFile filename source =
    writeFile filename ( "{-# OPTIONS_GHC -F -pgmF inlitpp #-}\n" <> source )


write :: OutputDirectory -> [RenderedData] -> IO ()
write (OutputDirectory outputDir) renderedData =
  forM_ renderedData $ \RenderedData {..} -> do
    let filename = toString $ outputDir <> "/" <> renderedDataFile
    createDirectoryIfMissing True ( toString outputDir )
    writeFile filename renderedDataContent



