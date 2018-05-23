module Tintin.Domain.Project where

import Tintin.Core


data Project

data Color
  = Purple
  | LightGreen
  | DarkGreen
  | Blue
  | DarkBlue
  | Bronze
  | DarkOrange
  | LightOrange
  | Red
  | Grey
  deriving (Show, Read)


data Page = Page
  { title    :: Text
  , content  :: Text
  , filename :: Text
  }


data Info = Info
  { name         :: Text
  , synopsis     :: Text
  , color        :: Color
  , githubLink   :: Text
  , githubAuthor :: Text
  , logoUrl      :: Maybe Text
  , pages        :: [Page]
  }

