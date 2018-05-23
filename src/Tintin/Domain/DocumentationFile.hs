module Tintin.Domain.DocumentationFile where

import qualified Data.Frontmatter as FMParser

import Tintin.Core

require Tintin.Domain.FrontMatter


newtype Filename   = Filename Text
newtype ParseError = ParseError Text deriving Show

data DocumentationFile = DocumentationFile
  { filename    :: Text
  , content     :: Text
  , frontMatter :: FrontMatter
  } deriving Show


new :: Filename -> Text -> Either ParseError DocumentationFile
new (Filename filename) rawText =
  case parse rawText of
    FMParser.Fail _ _ err ->
      Left ( ParseError $ "Parse error on "
             <> filename
             <> " no front matter found."
           )

    FMParser.Done source frontMatter ->
      Right $ DocumentationFile
        { filename    = filename
        , content     = decodeUtf8 @Text @ByteString source
        , frontMatter = frontMatter
        }
 where
  parse txt =
    encodeUtf8 @Text @ByteString txt
    |> FMParser.parseYamlFrontmatter


