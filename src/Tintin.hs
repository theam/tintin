module Tintin where

import Universum
import qualified Data.List.NonEmpty as NonEmpty

import qualified Data.Text as Text
import System.Directory
import System.IO.Temp
import System.Process


type Logger = (Text -> IO ())


makeLogger :: Bool -> Logger
makeLogger shouldLog msg =
  when shouldLog (putStrLn msg)


newtype DocumentationDirectory = DocumentationDirectory Text
newtype OutputDirectory        = OutputDirectory Text
newtype TemporaryDirectory     = TemporaryDirectory Text
newtype FrontMatter            = FrontMatter Text

data MarkdownFiles = MarkdownFiles
  { markdownFilesList :: [Text]
  , markdownFilesDirectory :: Text
  }

data HaskellFiles = HaskellFiles
  { haskellFilesData      :: [HaskellFile]
  , haskellFilesDirectory :: Text
  }

data HaskellFile = HaskellFile
  { haskellFileFrontMatter :: Text
  , haskellFileContent     :: Text
  , haskellFileName        :: Text
  }

data RenderedData = RenderedData
  { renderedDataContent :: Text
  , renderedDataFile    :: Text
  }


deleteOutputDirectoryIfExists :: OutputDirectory -> IO ()
deleteOutputDirectoryIfExists (OutputDirectory outputDir) = do
  outputDirExists <- doesDirectoryExist ( toString outputDir )
  when outputDirExists (removeDirectoryRecursive $ toString outputDir)


getMarkdownFilesFrom :: DocumentationDirectory -> IO MarkdownFiles
getMarkdownFilesFrom ( DocumentationDirectory path ) = do
  files <- listDirectory $ toString path
  let markdownFiles = filter (Text.isSuffixOf ".md") $ toText <$> files
  return MarkdownFiles
    { markdownFilesList = markdownFiles
    , markdownFilesDirectory = path
    }


convertToHaskellFiles :: TemporaryDirectory -> MarkdownFiles -> IO HaskellFiles
convertToHaskellFiles ( TemporaryDirectory tmpdir ) MarkdownFiles {..} = do
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
    tail . NonEmpty.fromList . Text.splitOn "---"


changeExtensionTo :: Text -> Text -> Text
changeExtensionTo filename newExtension =
  fst ( Text.breakOn "." filename ) <> newExtension


renderHaskellFiles :: HaskellFiles -> IO [RenderedData]
renderHaskellFiles HaskellFiles {..} = forM haskellFilesData $ \HaskellFile {..} -> do
  let filename = toString $ haskellFilesDirectory <> "/" <> haskellFileName
  writeHaskellFile filename haskellFileContent
  rendered <- readProcess "stack" ["runghc", filename, "--", "--no-inlit-wrap"] ""
  let htmlFileName = haskellFileName `changeExtensionTo` ".html"
  let header = "---" <> haskellFileFrontMatter <> "layout: page\n---\n"
  return RenderedData
    { renderedDataContent = header <> toText rendered
    , renderedDataFile = Text.toLower htmlFileName
    }
 where
  writeHaskellFile filename source =
    writeFile filename ( "{-# OPTIONS_GHC -F -pgmF inlitpp #-}\n" <> source )


writeHtmlFiles :: OutputDirectory -> [RenderedData] -> IO ()
writeHtmlFiles (OutputDirectory outputDir) renderedData =
  forM_ renderedData $ \RenderedData {..} -> do
    let filename = toString $ outputDir <> "/" <> renderedDataFile
    createDirectoryIfMissing True ( toString outputDir )
    writeFile filename renderedDataContent


runApp :: Logger -> OutputDirectory -> IO ()
runApp doLog (OutputDirectory outputDir) = withSystemTempDirectory "tintin" $ \tmpdir -> do

  doLog "Deleting output directory if it exists"
  deleteOutputDirectoryIfExists (OutputDirectory outputDir)

  currentDir    <- toText <$> getCurrentDirectory

  doLog "Parsing Markdown files"
  markdownFiles <- getMarkdownFilesFrom (DocumentationDirectory $ currentDir <> "/doc/")

  doLog "Converting to Haskell files"
  haskellFiles  <- convertToHaskellFiles (TemporaryDirectory $ toText tmpdir) markdownFiles

  doLog "Compiling and rendering"
  renderedHtmlFiles <- renderHaskellFiles haskellFiles

  doLog "Writing to output"
  writeHtmlFiles (OutputDirectory outputDir) renderedHtmlFiles

  doLog ( "Website generated at " <> outputDir )

