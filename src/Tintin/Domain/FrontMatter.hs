module Tintin.Domain.FrontMatter where

import Tintin.Core

import Data.Yaml


data FrontMatter = FrontMatter
  { title :: Text
  , subtitle :: Maybe Text
  , layout :: Maybe Text
  } deriving Show


instance FromJSON FrontMatter where
  parseJSON (Object v) =
    FrontMatter
    <$> v .: "title"
    <*> v .:? "subtitle"
    <*> v .:? "layout"
  parseJSON _ = fail "Expected object"

