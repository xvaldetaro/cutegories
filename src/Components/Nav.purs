module Dumb.Nav where

import Prelude

import App.Route (Route)
import App.Route as Route
import Components.Dumb.Icon as Icon
import Data.Maybe (Maybe, fromMaybe)
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Platform.Html.CssUtils (css, cx)
import Platform.Html.Utils (safeHref)

nav :: ∀ props act. Maybe Route -> HH.HTML props act
nav mbRoute =
  HH.div
    [ HP.classes
        [ ClassName "h-14 container mx-auto px-0 flex flex-row items-stretch"
        , ClassName "justify-start mb-10 text-slate-400 font-semibold text-md"
        ]
    ]
    [ menuItem Route.Landing "Home" ""
    , menuItem Route.PlayerList "Games" ""
    , menuItem Route.CreatePlayer "Players" ""
    , menuItem Route.PlaygroundDummy "Playground" ""
    , menuItem Route.PlaygroundFrp "Frp" ""
    , Icon.menu [ Icon.classes $ [ ClassName "block md:hidden w-4" ] ]
    ]
    where
    currentRoute = fromMaybe Route.Landing mbRoute
    menuItem route text extraCss = HH.a
      [ safeHref route
      , HP.classes
        [ ClassName "hidden transition hover:text-teal-500 px-4 align-middle md:flex items-center"
        , ClassName extraCss
        , cx (ClassName "text-teal-500") (currentRoute == route)
        ]
      ]
      [ HH.text text ]