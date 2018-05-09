module Tintin.Html.Templating where

import Lucid

import Tintin.Core
import qualified Tintin.Html.Style as Style



wrapPage :: [RenderedData] -> RenderedData -> Text
wrapPage _ RenderedData {..} = toText . renderText $ do
  h1_ $ toHtmlRaw renderedDataTitle
  div_ [class_ "content"] $ toHtml renderedDataContent


wrapHome :: [RenderedData] -> RenderedData -> Text
wrapHome pages rd = toText . renderText $ do
  tintinHeader
  nav_ [ class_ "navbar navbar-expand-lg navbar-dark tintin-navbar position-absolute"
         , style_ "bottom:0; width: 100%;"
         ] $ do
    div_ [class_ "collapse navbar-collapse", id_ "navbarSupportedContent"] $ do
      ul_ [class_ "navbar-nav mr-auto"] $ do
        li_ [class_ "nav-item active"] $ do
          a_ [class_ "nav-link active", href_ "/index.html"] "Home"
        forM (filter (\x -> "index.html" /= renderedDataFile x ) pages ) $ \page -> do
          li_ [class_ "nav-item"] $ do
            a_ [class_ "nav-link", href_ ( renderedDataFile page )] (toHtml $ renderedDataTitle page)
    div_ $ do
      ul_ [class_ "navbar-nav mr-sm-2"] $ do
        li_ [class_ "nav-item"] $ do
          a_ [class_ "nav-link", href_ "https://github.com/theam/tintin"] "GitHub"

  div_ [class_ "cover-container d-flex h-100 p-3 mx-auto flex-column"] $ do
    main_ [role_ "main", class_ "masthead mb-auto h-100"] $ do
      div_ [class_ "container h-100"] $ do
        div_ [class_ "row h-100 align-items-center"] $ do
          div_ [class_ "col"] $ do
            h1_ [class_ "cover-heading"] $ toHtml ( renderedDataTitle rd )
            h2_ "Document your package, before asking Haddock"
          div_ [class_ "col"] $ do
            div_ [class_ "d-flex justify-content-center"] $
              pre_ [style_ "font-size: 900%; color: #de935f;"] ")``)"
  section_ [ id_ "content" ] $ do
    div_ [class_ "container"] $ do
      div_ [class_ "content"] $
        div_ [class_ "rawr"] $ do
          toHtmlRaw $ renderedDataContent rd
  footer_ [] $ do
    div_ [class_ "container"] $ do
      div_ [class_ "row"] $ do
        div_ [class_ "col"] $ do
          p_ "Developed by @theam"
          p_ "Built with tintin"
        div_ [class_ "col"] $ do
          a_ [href_ "https://github.com/theam/tintin"] "Fork me on Github!"


tintinHeader :: Html ()
tintinHeader = do
  link_ [ rel_ "stylesheet"
        , href_ "https://fonts.googleapis.com/css?family=Karla|Montserrat:700"
        ]
  link_ [ rel_ "stylesheet"
        , href_ "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
        ]
  script_ [ src_ "https://code.jquery.com/jquery-3.2.1.slim.min.js" ] ( "" :: Text )
  script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js"] ( "" :: Text )
  script_ [ src_ "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js"] ( "" :: Text )
  style_ Style.style

