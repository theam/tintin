module Tintin.Domain.DocumentationFile where

import Tintin.Core

import qualified Data.Text as Text
import qualified Data.Frontmatter as FMParser

import Tintin.Domain.FrontMatter as FrontMatter


newtype Filename   = Filename Text
newtype ParseError = ParseError Text deriving Show

data Value = Value
  { filename    :: Text
  , content     :: Text
  , frontMatter :: FrontMatter
  } deriving Show


new :: Filename -> Text -> Either ParseError Value
new (Filename filename) rawText =
  case parse rawText of
    FMParser.Fail _ _ err ->
      Left ( ParseError $ "Parse error on "
             <> filename
             <> " no front matter found."
           )

    FMParser.Done source frontMatter ->
      Right $ Value
        { filename    = filename
        , content     = decodeUtf8 @Text @ByteString source
        , frontMatter = frontMatter
        }
 where
  parse txt =
    encodeUtf8 @Text @ByteString txt
    |> FMParser.parseYamlFrontmatter


