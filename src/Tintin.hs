module Tintin where

import Control.Monad
import Options.Generic
import GHC.Generics
import Data.Text as Text
import Data.Text.IO as Text
import System.Directory
import System.IO.Temp
import Data.Monoid
import System.Process

data Options = Options
  { outputDirectory :: Maybe Text
  , verbose :: Bool
  } deriving (Generic)

instance ParseRecord Options


newtype DocumentationDirectory = DocumentationDirectory Text
newtype OutputDirectory        = OutputDirectory Text


makeLogger :: Bool -> (String -> IO ())
makeLogger shouldLog msg = do
  when shouldLog (print msg)
  return ()


runApp :: Options -> IO ()
runApp Options {..} = withSystemTempDirectory "tintin" $ \tmpdir -> do
  let log       = makeLogger verbose
  let outputDir = Text.unpack $ maybe ("dist/tintin/") id outputDirectory
  outputDirExists <- doesDirectoryExist outputDir

  when outputDirExists (removeDirectoryRecursive outputDir)

  currDir <- getCurrentDirectory
  files <- listDirectory "doc"
  let mdFiles = fmap Text.unpack $ Prelude.filter (isSuffixOf ".md") $ fmap Text.pack $ files

  forM_ mdFiles $ \mdFile -> do
    content <- Text.readFile (currDir <> "/doc/" <> mdFile )
    unless ("---" `isPrefixOf` content) (error $ "File " <> mdFile <> " must start with a front-matter.")
    let [ _, header, source ] = splitMarkdown content
    let hsFile =
          tmpdir <> "/"
          <> ( Text.unpack $ ( fst $ breakOn "." $ Text.pack mdFile ) )
          <> ".hs"
    Text.appendFile hsFile "{-# OPTIONS_GHC -F -pgmF inlitpp #-}"
    Text.appendFile hsFile source

    log ("Rendering file " <> hsFile)
    rendered <- readProcess "stack" ["runghc", hsFile, "--", "--no-inlit-wrap"] ""
    createDirectoryIfMissing True outputDir
    let htmlFile =
          outputDir <> "/"
          <> ( Text.unpack $ ( toLower $ fst $ breakOn "." $ Text.pack mdFile ) )
          <> ".html"
    Text.appendFile htmlFile "---"
    Text.appendFile htmlFile header
    Text.appendFile htmlFile "layout: page\n---\n"
    Text.appendFile htmlFile ( Text.pack rendered )

  Prelude.putStrLn ( "Website generated at " <> outputDir )

splitMarkdown :: Text -> [Text]
splitMarkdown = splitOn "---"
