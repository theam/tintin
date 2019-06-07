module Tintin.Html.Style where

import Tintin.Core as Core hiding (( & ), rem, (|>))

import Clay
import qualified Clay.Media as Media

require Tintin.Domain.Project

data Style

style :: Project.Info -> Text
style info = toText . render $ do
  let (themeColorName, themeColorCode) = themeColor $ Project.color info
  
  html ? do
    height (pct 100)
    minHeight (pct 100)

  body ? do
    height (pct 100)
    minHeight (pct 100)
    fontFamily [Project.bodyFont info] [sansSerif]
    fontSize (em 1)
    overflowX hidden
    
    "a" ? do
      color $ shade 0.3 themeColorCode  
    "a:hover" ? do
      color $ shade 0.1 themeColorCode

  forM_ (zip [(0::Double)..] [h1, h2, h3]) $ \(n, x) -> x ? do
    fontFamily [Project.titleFont info] [sansSerif]
    fontWeight $ weight $ Project.titleFontWeight info
    fontSize (em (2.441 Core.** n))

  h1 ? fontSize (em 2.441)
  h2 ? fontSize (em 1.953)
  h3 ? fontSize (em 1.563)

  blockquote ? do
    borderLeft solid (px 4) "#DDD"
    paddingLeft (rem 1)
    color codeTextColor

  ".next-prev" ? do
    marginTop (pct 5)
    marginBottom (pct 5)

  "#header-container" ? do
    marginTop (rem 5)
    marginBottom (rem 5)

  ".cover-heading" ? do
    fontSize (pct 800)
    maxHeight (rem 6)
    marginBottom (rem 1.563)

  ".cover-container" ? do
   backgroundColor themeColorCode

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
    color $ contrastingColorFor themeColorCode

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
        color $ shade 0.6 themeColorCode
        fontSize (em 1.1)
      "a:hover" ? do
        textDecoration none
        color $ shade 0.7 themeColorCode
      ".tintin-fg-active" ? do
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

  ".filter-gray" ? do
    position relative
    bottom (px ( 3 ))
    marginLeft (rem 0.25)
    marginRight (rem 1)
    height (rem 1)

  ".footer-theam" ? do
    position relative
    bottom (px $ 1)
    marginLeft (rem $ -0.25)
    height (rem 1.75)

  ".tintin-doc-footer" ? do
    bottom (px 0)
    height (rem $ -15)
    width (pct 100)
    color footerTextColor

  ".main-container" ? do
    minHeight (pct 100)
    position relative

  "#content" ? do
    minHeight (pct 95)

  ".sidebar-nav" |> ".sidebar-brand" ? do
    height (rem 3)
    fontSize (rem 2)
    fontFamily [Project.titleFont info] [sansSerif]
    fontWeight $ weight $ Project.titleFontWeight info
    paddingTop (rem 2.5)
    paddingBottom (rem 2.5)
    marginBottom (rem 1.5)
    img ? do
      height (rem 1.5)

  ".tintin-navbar" ? do
    fontWeight bold
    backgroundColor $ shade 0.2 themeColorCode
    ".left-part" ? do
      important $ paddingLeft (px 0)
    ul ? do
      marginTop (rem 1)
      listStyleType none
      li ? do
        marginRight (rem 1)
        display inline
        a ? do
          color $ shade 0.6 themeColorCode
        "a:hover" ? do
          textDecoration none
          color $ shade 0.7 themeColorCode

    ".tintin-navbar-active" ? do
      a ? do
        color $ contrastingColorFor themeColorCode
      "a:hover" ? do
        textDecoration none
        color . shade 0.2 $ contrastingColorFor themeColorCode

  ".tintin-bg-70" ? do
    backgroundColor $ shade 0.2 themeColorCode
 
  (element $ ".tintin-bg-" <> themeColorName) ? do
    backgroundColor themeColorCode

  (element $ ".tintin-fg-" <> themeColorName) ? do
    color themeColorCode

  ".tintin-fg-active" ? do
    color white


  ".tintin-fg-disabled" ? do
    color black

  footer ? do
    position relative
    bottom (px 0)
    left (px 0)
    width (pct 100)
    paddingTop (px 30)
    paddingBottom (px 30)
    color $ contrastingColorFor themeColorCode
    backgroundColor $ shade 0.2 themeColorCode
    textAlign center
    
    a ? do
      color $ shade 0.6 themeColorCode
    "a:hover" ? do
      textDecoration none
      color $ shade 0.7 themeColorCode
      
    ".author" ? do
      marginTop (em 1.2)
      fontSize (em 1.2)
      
    ".site-generated-message" ? do
      fontSize (em 0.8)
      marginTop (em 3)
      marginBottom (em 3)

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


themeColor :: Project.Color -> (Text, Color)
themeColor (Project.HexColor colorCode) = ("custom"     , fromString $ toString colorCode)
themeColor Project.Purple               = ("purple"     , "#9F76B4")
themeColor Project.LightGreen           = ("lightgreen" , "#A4CB58")
themeColor Project.DarkGreen            = ("darkgreen"  , "#3C8B6A")
themeColor Project.Blue                 = ("blue"       , "#94C1E8")
themeColor Project.DarkBlue             = ("darkblue"   , "#007C99")
themeColor Project.Bronze               = ("bronze"     , "#A4A27A")
themeColor Project.DarkOrange           = ("darkorange" , "#FF6602")
themeColor Project.LightOrange          = ("lightorange", "#FAA73E")
themeColor Project.Red                  = ("red"        , "#D30228")
themeColor Project.Grey                 = ("grey"       , "#4D4D4D")

codeTextColor :: Color
codeTextColor = "#777"

blackish :: Color -- This is the default dark grey color from Boostrap
blackish = "#212529"

footerTextColor :: Color
footerTextColor = rgba 0 0 0 0.30

contrastingColorFor :: Color -> Color
contrastingColorFor color = 
  case soulFor color of
    Light    -> blackish
    Darkness -> white
    
shade :: Float -> Color -> Color
shade percent color = 
  case soulFor color of
    Light    -> Clay.darken percent color
    Darkness -> Clay.lighten percent color

data Soul = Darkness | Light

soulFor :: Color -> Soul
soulFor (Clay.Rgba red green blue _) =
  if ((fromIntegral (red + green + blue) / 3) > midTone)
  then Light
  else Darkness
 where
  midTone = 128.0
soulFor c = soulFor $ Clay.toRgba c