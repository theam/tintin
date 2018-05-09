module Tintin.Html.Style where

import Tintin.Core

import Clay

style :: Text
style = toText . render $ do
  body ? do
    backgroundColor white'
    color black'
    fontFamily ["Karla"] [ sansSerif ]

  forM [h1, h2, h3] $ \x -> x ? do
    fontFamily  ["Montserrat" ] [ sansSerif ]

  ".cover-heading" ? do
    fontSize (pct 800)

  ".vertical-auto" ? do
    marginTop auto
    marginBottom auto

  ".cover-container" ? do
    backgroundColor blue'
    color white'

  ".content" ? do
    marginTop (pct 5)
    marginBottom (pct 5)

  ".tintin-navbar" ? do
    fontWeight bold
    backgroundColor lightblue'

  footer ? do
    backgroundColor darkgrey'
    color white'
    paddingTop (px 30)
    paddingBottom (px 30)

  footer |> a ? do
    color lightgrey'


black'      = "#1d1f21"
white'      = "#f5f8f6"
darkwhite'  = "#c5c8c6"
darkgrey'   = "#282a2e"
grey'       = "#373b41"
red'        = "#a54242"
lightred'   = "#cc6666"
green'      = "#8c9440"
lightgreen' = "#b5bd68"
orange'     = "#de935f"
yellow'     = "#f0c674"
blue'       = "#5f819d"
lightblue'  = "#81a2be"
purple'     = "#85678f"
pink'       = "#b294bb"
cyan'       = "#5e8d87"
lightcyan'  = "#8abeb7"
lightgrey'  = "#707880"
