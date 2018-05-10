module Tintin.Html.Style where

import Tintin.Core hiding (( & ))

import Clay
import qualified Clay.Media as Media
import Clay.Selector

style :: Text
style = toText . render $ do
  body ? do
    fontFamily ["Karla"] [ sansSerif ]
    overflowX hidden

  forM [h1, h2, h3] $ \x -> x ? do
    fontFamily  ["Montserrat" ] [ sansSerif ]

  a ? do
    let ( ( _, colorValue ) : _ ) = Tintin.Core.filter (\x -> fst x == "lightblue") colorNames
    color colorValue
    ":hover" Clay.& color colorValue


  ".cover-heading" ? do
    fontSize (pct 800)

  ".vertical-auto" ? do
    marginTop auto
    marginBottom auto

  ".content" ? do
    marginTop (pct 5)
    marginBottom (pct 5)

  "#wrapper" ? do
    paddingLeft (px 0)
    transition "all" (sec 0.5) ease (sec 0.5)
    ".toggled" & do
      paddingLeft (px 250)
      "#sidebar-wrapper" ? do
        width (px 250)
      "#page-content-wrapper" & do
        position absolute
        marginRight (px (-250))

  "#page-content-wrapper" ? do
    width (pct 100)
    position absolute
    padding (px 15) (px 15) (px 15) (px 15)

  "#sidebar-wrapper" ? do
    zIndex 1000
    position fixed
    left (px 250)
    width (px 0)
    marginLeft (px (-250))
    overflowY auto
    transition "all" (sec 0.5) ease (sec 0.5)


  ".sidebar-nav" ? do
    position absolute
    top (px 0)
    width (px 250)
    margin (px 0) (px 0) (px 0) (px 0)
    padding (px 0) (px 0) (px 0) (px 0)
    listStyleType none
    "li" ? do
      textIndent (indent $ px 20)
      lineHeight (px 40)
      "a" ? do
        display block
        textDecoration none
        fontWeight bold
        ":hover" & do
          textDecoration none
          color white
        ":active" & do
          textDecoration none
          color white
        ":focus" & do
          textDecoration none
          color white

  ".sidebar-nav" |> ".sidebar-brand" ? do
    height (px 65)
    fontSize (px 28)
    lineHeight (px 60)
    fontFamily ["Montserrat"] [sansSerif]
    fontWeight bold


  ".tintin-navbar" ? do
    fontWeight bold

  forM colorNames $ \(colorName, colorValue) ->
    (text $ ".tintin-bg-" <> colorName) ? do
      backgroundColor colorValue

  forM colorNames $ \(colorName, colorValue) ->
    (text $ ".tintin-fg-" <> colorName) ? do
      color colorValue

  footer ? do
    paddingTop (px 30)
    paddingBottom (px 30)

  query Media.screen [Media.minWidth (px 768)] $ do
    "#wrapper" ? do
      paddingLeft (px 0)
      ".toggled" ? do
        paddingLeft (px 250)
        "#sidebar-wrapper" ? do
          width (px 250)
        "#page-content-wrapper" ? do
          position relative
          marginRight (px 0)

    "#sidebar-wrapper" ? do
      width (px 0)

    "#page-content-wrapper" ? do
      padding (px 20) (px 20) (px 20) (px 20)
      position relative




colorNames :: [(Text, Color)]
colorNames =
  [ ("black"     , "#1d1f21")
  , ("white"     , "#f5f8f6")
  , ("darkwhite" , "#c5c8c6")
  , ("darkgrey"  , "#282a2e")
  , ("grey"      , "#373b41")
  , ("red"       , "#a54242")
  , ("lightred"  , "#cc6666")
  , ("green"     , "#8c9440")
  , ("lightgreen", "#b5bd68")
  , ("orange"    , "#de935f")
  , ("yellow"    , "#f0c674")
  , ("blue"      , "#5f819d")
  , ("lightblue" , "#81a2be")
  , ("purple"    , "#85678f")
  , ("pink"      , "#b294bb")
  , ("cyan"      , "#5e8d87")
  , ("lightcyan" , "#8abeb7")
  , ("lightgrey" , "#707880")
  ]
