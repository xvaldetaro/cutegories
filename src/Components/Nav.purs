module Dumb.Nav where

import Prelude

import Components.Dumb.Icon as Icon
import Core.HTMLUtils (safeHref)
import Core.Route (Route)
import Core.Route as Route
import Data.Maybe (Maybe, fromMaybe)
import HTML.Utils (css, cx)
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

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