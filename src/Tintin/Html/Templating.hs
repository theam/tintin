module Tintin.Html.Templating where

import Lucid

import Data.Maybe
import Tintin.Core
require Tintin.Html.Style
require Tintin.Domain.Project

require Data.Text

data Templating


asset :: Text -> Text
asset txt = "https://s3-eu-west-1.amazonaws.com/worldwideapps/assets/" <> txt

wrap :: Project.Info -> Project.Context -> Project.Page -> Text
wrap info context page =
  if (Project.filename page) == "index.html"
  then wrapHome info (Project.nextRef context) page
  else wrapPage info context page


wrapPage :: Project.Info -> Project.Context -> Project.Page -> Text
wrapPage info context page = toText . renderText $ do
  doctypehtml_ $ do
    tintinHeader info page
    body_ [class_ "h-100 tintin-fg-black tintin-bg-white"] $ do
      div_ [id_ "main-container", class_ "h-100"] $ do
        section_ [ id_ "content"] $ do
          div_ [id_ "wrapper", class_ "toggled"] $ do
            div_ [id_ "sidebar-wrapper", class_ $ "h-100 tintin-bg-" <> bgColorNameOf info] $ do
              div_ [ class_ "h-100 tintin-bg-70"] ( p_ "" )
              ul_ [ class_ "sidebar-nav"] $ do
                li_ [ class_ $ "sidebar-brand d-flex tintin-bg-" <> bgColorNameOf info] $ do
                  a_ [href_ "index.html", class_ "align-self-center tintin-fg-white"] $ do
                    case Project.logoUrl info of
                      Nothing -> toHtml $ Project.name info
                      Just url -> img_ [src_ url]
                forM_ (filter (\p -> "index.html" /= Project.filename p ) (Project.pages info) ) $ \p -> do
                  li_ $ do
                    let classes =
                          if Project.title page == Project.title p
                          then "tintin-fg-active"
                          else "tintin-fg-disabled"
                    a_ [href_ $ Project.filename p, class_ classes] $ do
                      ( toHtml $ Project.title p )

            nav_ [class_ "navbar navbar-expand-lg tintin-doc-topbar tintin-fg-white"] $ do
              a_ [id_ "menu-toggle", href_ "#menu-toggle", class_ ""] $ do
                img_ [ src_ $ asset "menu.png", class_ "img-fluid animated rotateOut" ]
                img_ [ src_ $ asset "close.png", class_ "img-fluid animated rotateIn" ]

            div_ [id_ "page-content-wrapper"] $ do
              div_ [class_ "container"] $ do
                div_ [class_ "col"] $
                  div_ [ class_ "animated fadeIn"
                       ] $
                    toHtmlRaw $ Project.content page
        div_ [class_ "tintin-doc-footer clear-fix"] $ do
          nextPrev context
          footer info
      tintinPostInit

nextPrev :: Project.Context -> Html ()
nextPrev context = do
  div_ [class_ "row next-prev"] $ do
    whenJust (Project.prevRef context) $ \prev -> do
      div_ [class_ "col-md-4 offset-md-4"] $ do
        a_ [href_ $ Project.refFilename prev] $  do
          ( toHtml $ "< Previous: " <> Project.refTitle prev )
    whenJust (Project.nextRef context) $ \next -> do
      div_ [class_ "col-md-4 ml-auto"] $ do
        a_ [href_ $ Project.refFilename next] $  do
          ( toHtml $ "Next: " <> Project.refTitle next <> " >")

footer :: Project.Info -> Html ()
footer info =
  footer_ [] $
    div_ [class_ "container"] $
      div_ [class_ "row"] $ 
        div_ [class_ "col"] $ do
          when (isJust $ Project.author info) $  
            p_ [class_ "author"] $ do
              "Developed by "
              b_ [] $
                a_ [ href_ $ fromJust $ Project.authorWebsite info
                   , target_ "blank"
                   ] (toHtml $ fromJust $ Project.author info)
          p_ [class_ "site-generated-message"] $ do
            "Site generated with "
            a_ [ href_ "https://theam.github.io/tintin", class_ "tintin-logo" ] $
              img_ [ class_ "filter-gray", src_ $ asset "logo.svg" ]
            span_ [] $ toHtmlRaw ("&mdash; &copy; 2019 " :: Text)
            a_ [ href_ "https://www.theagilemonkeys.com"] "The Agile Monkeys"


wrapHome :: Project.Info -> Maybe Project.PageRef -> Project.Page -> Text
wrapHome info nextRef page = toText . renderText $ do
  doctypehtml_ $ do
    tintinHeader info page
    body_ [class_ "tintin-fg-black tintin-bg-white"] $ do

      div_ [class_ $ "tintin-navigation tintin-bg-" <> bgColorNameOf info] $ do
        div_ [class_ "cover-container d-flex p-3 mx-auto flex-column tintin-fg-white"] $ do
          main_ [role_ "main", class_ "masthead mb-auto"] $ do
            div_ [class_ "container"] $ do
              div_ [class_ "row align-items-center"] $ do
                div_ [class_ "col", id_ "header-container"] $ do
                  case Project.logoUrl info of
                    Just url -> img_ [ src_ url, class_ "cover-heading" ]
                    Nothing -> h1_ [class_ "cover-heading"] $ toHtml (Project.name info)
                  p_ [class_ "cover-heading-subtitle"] (toHtml $ Project.synopsis info)

        navbar

      section_ [ id_ "content"
               , class_ "animated fadeIn"
               ] $ do
        div_ [class_ "container"] $ do
          div_ [class_ "content"
               ] $
            div_ [] $ do
              toHtmlRaw $ Project.content page
      nextPrev (Project.Context Nothing nextRef)
      footer info
      tintinPostInit
 where
  navbar =
    nav_ [ class_ "tintin-navbar"
         , style_ "width: 100%;"
         ] $ do
      div_ [ class_ "container d-flex align-items-center" ] $ do
        div_ [class_ "mr-auto"] $ do
          ul_ [class_ "left-part"] $ do
            li_ [class_ "tintin-navbar-active"] $
              a_ [class_ "", href_ "index.html"] "Home"
            let (page:_) = filter (\x -> "index.html" /= Project.filename x ) (Project.pages info)
            li_ [class_ ""] $
              a_ [class_ "", href_ (Project.filename page)] "Docs"
        whenJust (Project.githubLink info) $ \ghlink ->
          div_ [class_ ""] $
            ul_ [class_ ""] $
              li_ [class_ ""] $
                a_ [class_ "", href_ $ "https://github.com/" <> ghlink] "View on GitHub"


tintinHeader :: Project.Info -> Project.Page -> Html ()
tintinHeader info@Project.Info {..} Project.Page {..} =
  head_ $ do
    title_ ( toHtml $ name <> " - " <> title )
    -- Workaround over https://github.com/highlightjs/highlight.js/issues/1387
    meta_ [ charset_ "utf-8" ]
    -- Twitter Card data
    meta_ [ name_ "twitter:card"
          , content_ "summary"
          ]
    meta_ [ name_ "twitter:site"
          , content_ ("https://s3-eu-west-1.amazonaws.com/worldwideapps/assets/tintin-" <> (Text.toLower $ show color) <> ".png")

          ]
    meta_ [ name_ "twitter:title"
          , content_ (name <> " - " <> title)
          ]
    meta_ [ name_ "twitter:description"
          , content_ synopsis
          ]
    whenJust author $ \author ->
      meta_ [ name_ "twitter:creator"
            , content_ author
            ]
    meta_ [ name_ "twitter:image"
          , content_ ("https://s3-eu-west-1.amazonaws.com/worldwideapps/assets/tintin-" <> (Text.toLower $ show color) <> ".png")

          ]
    -- Open Graph data
    meta_ [ itemprop_ "og:type"
          , content_ "website"
          ]
    meta_ [ itemprop_ "og:site_name"
          , content_ "theam"
          ]
    meta_ [ itemprop_ "og:title"
          , content_ (name <> " - " <> title)
          ]
    whenJust githubLink $ \ghlink ->
      meta_ [ itemprop_ "og:url"
            , content_ ghlink
            ]
    meta_ [ itemprop_ "og:image"
          , content_  ("https://s3-eu-west-1.amazonaws.com/worldwideapps/assets/tintin-" <> (Text.toLower $ show color) <> ".png")

          ]
    meta_ [ itemprop_ "og:description"
          , content_ synopsis
          ]
    link_ [ rel_ "stylesheet"
          , href_ $ googleFontUrl info
          ]
    link_ [ rel_ "stylesheet"
          , href_ "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
          ]
    link_ [ rel_ "stylesheet"
          , href_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/tomorrow-night.min.css"
          ]
    link_ [ rel_ "stylesheet"
          , href_ "https://cdnjs.cloudflare.com/ajax/libs/animate.css/3.5.2/animate.min.css"
          ]
    link_ [ rel_ "shortcut icon"
          , href_ ( asset $ "favicon-"
                  <> bgColorNameOf info
                  <> ".ico"
                  )
          ]
    link_ [ rel_ "stylesheet"
          , href_ "https://cdn.jsdelivr.net/npm/katex@0.10.0-alpha/dist/katex.min.css"
          ]
    style_ $ Style.style info


tintinPostInit :: Html ()
tintinPostInit = do
  script_ [ src_ "https://code.jquery.com/jquery-3.2.1.slim.min.js" ] ( "" :: Text )
  script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js"] ( "" :: Text )
  script_ [ src_ "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js"] ( "" :: Text )
  script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"] ( "" :: Text )
  script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/haskell.min.js"] ("" :: Text)
  script_ [ src_ "https://cdn.rawgit.com/icons8/bower-webicon/v0.10.7/jquery-webicon.min.js" ] ( "" :: Text )
  script_ "hljs.initHighlightingOnLoad()"
  script_ "$(function () {$(\"#menu-toggle\").click(function(e) {e.preventDefault();$(\"#wrapper\").toggleClass(\"toggled\");$(\"#menu-toggle img\").toggleClass(\"rotateIn rotateOut\");})});"
  script_ [src_ "https://cdn.jsdelivr.net/npm/katex@0.10.0-alpha/dist/katex.min.js"] ("" :: Text)
  script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.9.0/contrib/auto-render.min.js"] ("" :: Text)
  script_ "renderMathInElement(document.body);"

bgColorNameOf :: Project.Info -> Text
bgColorNameOf info =
  case color of
    Project.HexColor color -> fromString "custom"
    _ -> color
          & show
          & Text.toLower
  where color = Project.color info


googleFontUrl :: Project.Info -> Text
googleFontUrl Project.Info {..} = 
  "https://fonts.googleapis.com/css?family=" <> bodyFont <> "|" <> titleFont <> ":" <> show titleFontWeight