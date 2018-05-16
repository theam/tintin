module Tintin.Domain.DocumentationFile where

import Tintin.Core

import qualified Data.Text as Text
import qualified Data.Frontmatter as FMParser

import Tintin.Domain.FrontMatter as FrontMatter


newtype Filename   = Filename Text
newtype ParseError = ParseError { errorText :: Text }

data Value = Value
  { filename    :: Filename
  , fileContent :: Text
  , frontMatter :: FrontMatter
  }


new :: Filename -> Text -> Either ParseError Value
new (Filename filename) rawText =
  case parse rawText of
    FMParser.Fail _ _ err ->
      Left (ParseError $ "Parse error on " <> filename <> ", error found: " <> toText err)

    FMParser.Done source frontMatter ->
      Right $ Value
        { filename    = Filename filename
        , fileContent = decodeUtf8 @Text @ByteString source
        , frontMatter = frontMatter
        }
 where
  parse txt =
    encodeUtf8 @Text @ByteString txt
    |> FMParser.parseYamlFrontmatter


