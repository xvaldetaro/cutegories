module Nuts.Nav where

import Prelude

import App.Route (Route)
import App.Route as Route
import Control.Alt ((<|>))
import Deku.Attribute ((:=))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import FRP.Event (Event)
import Platform.Deku.Html (bangCss', combineCss, css)
import Platform.Html.Utils (safeHref)

nut :: Event Route -> Nut
nut currentRouteEv =
  D.div
    ( bangCss'
      [ css "h-14 px-0 flex flex-row items-stretch justify-start"
      , css "font-semibold text-lg shadow-sm bg-gray-900 text-gray-300"
      ]
    )
    [ menuItem Route.Landing "Home" ""
    , menuItem Route.PlayerList "Games" ""
    , menuItem Route.CreatePlayer "Players" ""
    , menuItem Route.PlaygroundDummy "Playground" ""
    , menuItem Route.Debug "Debug" ""
    ]
    where
    menuItem route text extraCss = D.a
      ( (pure $ D.Href := safeHref route)
        <|> combineCss
          [ pure $ css "hidden transition hover:text-white hover:bg-gray-800 px-4 align-middle md:flex items-center"
          , currentRouteEv <#> (\r -> if r == route then css "text-white bg-gray-800" else "")
          ]
      )
      [ text_ text ]