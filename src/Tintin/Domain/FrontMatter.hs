module Tintin.Domain.FrontMatter where

import Tintin.Core

import Data.Yaml


data FrontMatter = FrontMatter
  { frontMatterTitle :: Text
  , frontMatterSubitle :: Maybe Text
  , frontMatterLayout :: Maybe Text
  }


instance FromJSON FrontMatter where
  parseJSON (Object v) =
    FrontMatter
    <$> v .: "title"
    <*> v .:? "subtitle"
    <*> v .:? "layout"
  parseJSON _ = fail "Expected object"

