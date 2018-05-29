module Tintin.Domain.HtmlFile where

import Tintin.Core
require Tintin.Capabilities.Filesystem
require Tintin.Capabilities.Process
require Tintin.Domain.DocumentationFile
require Tintin.Domain.FrontMatter
require Data.Text


data BuildTool
  = Stack
  | Cabal

newtype CompilationError = CompilationError Text deriving Show

showCompilationError :: CompilationError
                     -> Text
showCompilationError (CompilationError e) = "CompilationError\n" <> e

data HtmlFile = HtmlFile
  { filename :: Text
  , title    :: Text
  , content  :: Text
  }


fromDocumentationFile :: DocumentationFile
                      -> HtmlFile
fromDocumentationFile docfile =
  DocumentationFile.content docfile
   |> ("{-# OPTIONS_GHC -F -pgmF inlitpp #-}\n" <>)
   |> HtmlFile (DocumentationFile.filename docfile) docTitle
 where
  docTitle = docfile |> DocumentationFile.frontMatter |> FrontMatter.title


run :: ( Has Filesystem.Capability eff
       , Has Process.Capability eff
       )
    => BuildTool
    -> HtmlFile
    -> Effectful eff (Either CompilationError HtmlFile)
run buildTool HtmlFile {..} = do
  Filesystem.Path currentDirectory <- Filesystem.currentDirectory
  let tintinDir = currentDirectory <> "/.stack-work/tintin/"
  let tempDir   = tintinDir <> "temp/"
  let hsFilename = filename
                   |> Text.breakOn ".md"
                   |> fst
                   |> (<> ".hs")
                   |> (tempDir <>)
  let htmlFilename = filename
                     |> Text.breakOn ".md"
                     |> fst
                     |> (<> ".html")
  Filesystem.deleteIfExists (Filesystem.Path tempDir)
  Filesystem.makeDirectory (Filesystem.Path tempDir)
  Filesystem.writeFile (Filesystem.Path hsFilename) content
  result <- case buildTool of
              Stack -> Process.read
                       (Process.CommandName "stack")
                       (Process.Arguments ["runghc", hsFilename, "--", "--no-inlit-wrap"])
              Cabal -> Process.read
                       (Process.CommandName "runghc")
                       (Process.Arguments [hsFilename, "--no-inlit-wrap"])

  case result of
    Left  (Process.StdErr err) ->
      return (Left $ CompilationError err)

    Right (Process.StdOut msg) ->
      return . Right $ HtmlFile
        { filename = htmlFilename
        , content  = msg
        , title    = title
        }

