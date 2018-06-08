module Tintin.Html.Style where

import Tintin.Core as Core hiding (( & ), rem, (|>))

import Clay
import qualified Clay.Media as Media

data Style

style :: Text
style = toText . render $ do
  html ? do
    height (pct 100)
    minHeight (pct 100)

  body ? do
    height (pct 100)
    minHeight (pct 100)
    fontFamily ["IBM Plex Sans"] [ sansSerif ]
    fontSize (em 1)
    overflowX hidden

  forM_ (zip [(0::Double)..] [h1, h2, h3]) $ \(n, x) -> x ? do
    fontFamily ["Montserrat" ] [ sansSerif ]
    fontWeight bold
    fontSize (em (2.441 Core.** n))

  h1 ? fontSize (em 2.441)
  h2 ? fontSize (em 1.953)
  h3 ? fontSize (em 1.563)

  ".next-prev" ? do
    marginTop (fromInteger 5 :: Size Percentage)
    marginBottom (fromInteger 5 :: Size Percentage)

  "#header-container" ? do
    marginTop (rem 5)
    marginBottom (rem 5)

  ".cover-heading" ? do
    fontSize (pct 800)
    maxHeight (rem 6)
    marginBottom (rem 1.563)

  ".cover-container" ? do
   backgroundColor (rgba 255 255 255 0.0)

  ".watermark" ? do
    position absolute
    top (px 0)
    left (px 0)
    maxHeight (rem 1.2504)
    marginTop (rem 1.2504)
    marginLeft (rem 1.2504)

  ".cover-heading-subtitle" ? do
    marginTop (rem 3)
    fontSize (rem 1.953)

  ".vertical-auto" ? do
    marginTop auto
    marginBottom auto

  ".content" ? do
    marginTop (pct 5)
    marginBottom (pct 5)

  "#wrapper" ? do
    paddingLeft (px 0)
    transition "all" (sec 0.5) ease (sec 0)
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
    marginTop (rem 3)
    padding (px 15) (px 15) (px 15) (px 15)
    img ? do
      maxWidth (pct 100)

  "#sidebar-wrapper" ? do
    zIndex 1000
    position fixed
    left (px 250)
    width (px 0)
    marginLeft (px (-250))
    overflowY hidden
    overflowX hidden
    transition "all" (sec 0.5) ease (sec 0)


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
      ".tintin-fg-disabled" ? do
        ":hover" & do
           "mix-blend-mode" -: "normal"
           Clay.filter $ invert (pct 0)
           textDecoration none
           color black
      ".tintin-fg-active" ? do
        ":hover" & do
           "mix-blend-mode" -: "normal"
           Clay.filter $ invert (pct 0)
           textDecoration none
           color white

  "#menu-toggle" ? do
    position absolute
    img ? do
      position absolute
      left (px 0)
    ".rotateIn" ? do
      zIndex 999

  ".tintin-doc-topbar" ? do
    height (rem 3)
    a ? do
      marginLeft (rem 1)
      marginTop (rem 0)
      width (rem 1.5)
      img ? do
        Clay.filter (invert $ pct 70)

  ".filter-gray" ? do
    position relative
    bottom (px ( 3 ))
    marginLeft (rem 0.25)
    marginRight (rem 1)
    height (rem 1)
    Clay.filter (brightness 0.75)

  ".footer-theam" ? do
    position relative
    bottom (px $ 1)
    marginLeft (rem $ -0.25)
    height (rem 1.75)
    Clay.filter (invert (pct 75))

  ".tintin-doc-footer" ? do
    bottom (px 0)
    height (rem $ -15)
    width (pct 100)
    color (rgba 0 0 0 0.30)

  ".main-container" ? do
    minHeight (pct 100)
    position relative

  "#content" ? do
    minHeight (pct 95)

  ".sidebar-nav" |> ".sidebar-brand" ? do
    height (rem 3)
    fontSize (rem 2)
    fontFamily ["Montserrat"] [sansSerif]
    fontWeight bold
    paddingTop (rem 2.5)
    paddingBottom (rem 2.5)
    marginBottom (rem 1.5)
    img ? do
      height (rem 1.5)

  ".tintin-navbar" ? do
    fontWeight bold
    backgroundColor (rgba 255 255 255 0.15)
    ".left-part" ? do
      important $ paddingLeft (px 0)
    ul ? do
      marginTop (rem 1)
      listStyleType none
      li ? do
        marginRight (rem 1)
        display inline
        a ? do
          color black
          Clay.filter $ invert (pct 35)
          "mix-blend-mode" -: "difference"
        "a:hover" ? do
          "mix-blend-mode" -: "normal"
          Clay.filter $ invert (pct 0)
          textDecoration none
          color black

    ".tintin-navbar-active" ? do
      a ? do
        "mix-blend-mode" -: "normal"
        Clay.filter $ invert (pct 0)
        color white
      "a:hover" ? do
        "mix-blend-mode" -: "normal"
        Clay.filter $ invert (pct 0)
        textDecoration none
        color white

  ".tintin-bg-70" ? do
    backgroundColor (rgba 255 255 255 0.15)

  forM colorNames $ \(colorName, colorValue) ->
    (element $ ".tintin-bg-" <> colorName) ? do
      backgroundColor colorValue

  forM colorNames $ \(colorName, colorValue) ->
    (element $ ".tintin-fg-" <> colorName) ? do
      color colorValue

  ".tintin-fg-active" ? do
    color (rgba 255 255 255 1.0)


  ".tintin-fg-disabled" ? do
    color black
    Clay.filter $ invert (pct 35)
    "mix-blend-mode" -: "difference"

  footer ? do
    position relative
    bottom (px 0)
    left (px 0)
    width (pct 100)
    paddingTop (px 30)
    paddingBottom (px 30)

  ".container" ? do
    maxWidth (rem 50)

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
  [ ("black"      , "#1d1f21")
  , ("white"      , "#f5f8f6")
  , ("grey"       , "#4D4D4D")
  , ("red"        , "#D30228")
  , ("darkgreen"  , "#3C8B6A")
  , ("lightgreen" , "#A4CB58")
  , ("darkorange" , "#FF6602")
  , ("lightorange", "#FAA73E")
  , ("blue"       , "#94C1E8")
  , ("darkblue"   , "#007C99")
  , ("purple"     , "#9F76B4")
  , ("bronze"     , "#A4A27A")
  , ("darkgrey"   , "#282a2e")
  ]
