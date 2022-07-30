module Nuts.TopLevel where

import Prelude

import App.Env (AppNut)
import App.Navigation (navigate, routeChangeEvent)
import App.Route (Route)
import App.Route as Route
import Bolson.Core (Entity)
import Deku.Attribute ((:=))
import Deku.Control (switcher, text_)
import Deku.DOM as D
import FRP.Event (bang, fromEvent)
import Nuts.Landing (nut) as Landing
import Platform.Deku.Html (ezCss, ezClick)

nut :: AppNut
nut = pure $
  D.div (bang $ D.Class := "flex flex-col")
    [ D.div (ezCss "flex flex-row gap-4")
        [ D.div (ezClick $ navigate Route.CreatePlayer) [ text_ "CreateP" ]
        , D.div (ezClick $ navigate Route.Landing) [ text_ "Landing" ]
        ]
    , D.div (ezCss "container")
        [ switcher routeToChild (fromEvent routeChangeEvent)
        ]
    ]
  where
  routeToChild :: Route -> Entity _ _ _ _
  routeToChild = case _ of
    Route.Landing -> Landing.nut
    _ -> text_ "Route not available"