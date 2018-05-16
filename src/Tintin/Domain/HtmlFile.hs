module Tintin.Domain.HtmlFile where

import Tintin.Core
import qualified Tintin.Capabilities.Filesystem as Filesystem
import qualified Tintin.Capabilities.Process as Process
import qualified Tintin.Domain.DocumentationFile as DocumentationFile
import qualified Tintin.Domain.FrontMatter as FrontMatter

import qualified Data.Text as Text


newtype CompilationError = CompilationError Text deriving Show

data Value = Value
  { filename :: Text
  , title    :: Text
  , content  :: Text
  }


fromDocumentationFile :: DocumentationFile.Value
                      -> Value
fromDocumentationFile docfile =
  DocumentationFile.content docfile
   |> ("{-# OPTIONS_GHC -F -pgmF inlitpp #-}\n" <>)
   |> Value (DocumentationFile.filename docfile) docTitle
 where
  docTitle = docfile |> DocumentationFile.frontMatter |> FrontMatter.title


run :: ( Has Filesystem.Capability eff
       , Has Process.Capability eff
       )
    => Value
    -> Effectful eff (Either CompilationError Value)
run Value {..} = do
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
  result <- Process.read
    (Process.CommandName "stack")
    (Process.Arguments ["runghc", hsFilename, "--", "--no-inlit-wrap"])


  case result of
    Left  (Process.StdErr err) ->
      return (Left $ CompilationError err)

    Right (Process.StdOut msg) ->
      return . Right $ Value
        { filename = htmlFilename
        , content  = msg
        , title    = title
        }


