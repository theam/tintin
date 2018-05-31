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

wrap :: Project.Info -> Project.Page -> Text
wrap info page =
  if (Project.filename page) == "index.html"
  then wrapHome info page
  else wrapPage info page


wrapPage :: Project.Info -> Project.Page -> Text
wrapPage info page = toText . renderText $ do
  doctypehtml_ $ do
    tintinHeader info page
    body_ [class_ "h-100 tintin-fg-black tintin-bg-white"] $ do
      div_ [id_ "main-container", class_ "h-100"] $ do
        section_ [ id_ "content"] $ do
          div_ [id_ "wrapper", class_ "toggled"] $ do
            div_ [id_ "sidebar-wrapper", class_ $ "h-100 tintin-bg-" <> bgColorOf info] $ do
              div_ [ class_ "h-100 tintin-bg-70"] ( p_ "" )
              ul_ [ class_ "sidebar-nav"] $ do
                li_ [ class_ $ "sidebar-brand d-flex tintin-bg-" <> bgColorOf info] $ do
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
        div_ [class_ "tintin-doc-footer clear-fix"]
          siteGenerated
      tintinPostInit

siteGenerated = do
  div_ [class_ "float-right"] $ do
    div_ [class_ "d-inline", style_ "float: left"] $ do
      p_ [class_ ""] "Site generated with "
    a_ [ style_ "float: left", href_ "https://theam.github.io/tintin"] $ do
      img_ [ class_ "filter-gray", src_ $ asset "logo.svg" ]
    div_ [class_ "clear-fix float-right"] $ do
      span_ [style_ "float:left"] $ toHtmlRaw ("&mdash; &copy; 2018 " :: Text)
      a_ [class_ "float:left", href_ "http://theam.io"] $
        img_ [class_ "footer-theam", src_ "http://theam.io/logo_theam.png"]


wrapHome :: Project.Info -> Project.Page -> Text
wrapHome info page = toText . renderText $ do
  doctypehtml_ $ do
    tintinHeader info page
    body_ [class_ "tintin-fg-black tintin-bg-white"] $ do

      div_ [class_ $ "tintin-navigation tintin-bg-" <> bgColorOf info] $ do
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
      footer
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

  footer =
    footer_ [ class_ "tintin-bg-darkgrey tintin-fg-white"] $
      div_ [class_ "container"] $
        div_ [class_ "row"] $ do
          div_ [class_ "col"] $
            p_ [class_ "tintin-fg-lightgrey"] $
              when (isJust $ Project.githubLink info) $ do
              "Developed by "
              a_ [ href_ $ "https://github.com/" <> (fromJust $ Project.githubAuthor info)
                 , class_ $ "tintin-fg-" <> bgColorOf info
                 ] (toHtml $ fromJust $ Project.githubAuthor info)
          div_ [class_ "col", style_ ""] $ do
            siteGenerated


tintinHeader :: Project.Info -> Project.Page -> Html ()
tintinHeader info@Project.Info {..} Project.Page {..} =
  head_ $ do
    title_ ( toHtml $ name <> " - " <> title )
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
    whenJust githubAuthor $ \author ->
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
          , href_ "https://fonts.googleapis.com/css?family=IBM+Plex+Sans|Montserrat:500"
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
                  <> bgColorOf info
                  <> ".ico"
                  )
          ]
    link_ [ rel_ "stylesheet"
          , href_ "https://cdn.jsdelivr.net/npm/katex@0.10.0-alpha/dist/katex.min.css"
          ]
    style_ Style.style


tintinPostInit :: Html ()
tintinPostInit = do
  script_ [ src_ "https://code.jquery.com/jquery-3.2.1.slim.min.js" ] ( "" :: Text )
  script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js"] ( "" :: Text )
  script_ [ src_ "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js"] ( "" :: Text )
  script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"] ( "" :: Text )
  script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/haskell.min.js"] ("" :: Text)
  script_ [ src_ "https://cdn.rawgit.com/icons8/bower-webicon/v0.10.7/jquery-webicon.min.js" ] ( "" :: Text )
  script_ "hljs.initHighlightingOnLoad()"
  script_ "$(function () {$(\"#menu-toggle\").click(function(e) {\
        \e.preventDefault();\
        \$(\"#wrapper\").toggleClass(\"toggled\");\
        \$(\"#menu-toggle img\").toggleClass(\"rotateIn rotateOut\");\
    \})});"
  script_ [src_ "https://cdn.jsdelivr.net/npm/katex@0.10.0-alpha/dist/katex.min.js"] ("" :: Text)
  script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.9.0/contrib/auto-render.min.js"] ("" :: Text)
  script_ "renderMathInElement(document.body);"
bgColorOf :: Project.Info -> Text
bgColorOf info =
  Project.color info
  |> show
  |> Text.toLower

