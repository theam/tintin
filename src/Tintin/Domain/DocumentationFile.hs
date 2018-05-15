module Tintin.Domain.DocumentationFile where

import Tintin.Core

import qualified Data.Text as Text
import qualified Data.Frontmatter as FMParser

import Tintin.Domain.FrontMatter as FrontMatter


newtype Filename   = Filename Text
newtype Content    = Content Text
newtype ParseError = ParseError { errorText :: Text }

data DocumentationFile = DocumentationFile
  { filename    :: Filename
  , fileContent :: Content
  , frontMatter :: FrontMatter
  }


new :: Filename -> Text -> Either ParseError DocumentationFile
new (Filename filename) rawText =
  case parse rawText of
    FMParser.Fail _ _ err ->
      Left (ParseError $ "Parse error on " <> filename <> ", error found: " <> toText err)

    FMParser.Done source frontMatter ->
      Right $ DocumentationFile
        { filename    = Filename filename
        , fileContent = Content $ decodeUtf8 @Text @ByteString source
        , frontMatter = frontMatter
        }
 where
  parse txt =
    encodeUtf8 @Text @ByteString txt
    |> FMParser.parseYamlFrontmatter
