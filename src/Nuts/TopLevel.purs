module Nuts.TopLevel where

import Prelude

import App.Env (AppNut_, Nut_)
import App.Navigation (routeChangeEvent)
import App.Route (Route)
import App.Route as Route
import Bolson.Core (Entity)
import Control.Alt ((<|>))
import Deku.Attribute ((:=))
import Deku.Control (switcher, text_)
import Deku.DOM as D
import FRP.Event (AnEvent, bang)
import Nuts.Landing (nut) as Landing
import Paraglider.Rx (replayRefCount)
import Platform.Deku.Html (bangCss, bangCss', css, fragCss)
import Platform.Deku.Misc (usingEffect)
import Platform.Html.Utils (safeHref)

nut :: ∀ s m l p. AppNut_ s m l p
nut = pure $ usingEffect (replayRefCount routeChangeEvent) \currentRouteEv ->
  closureWithRouteEv currentRouteEv

  where
  closureWithRouteEv :: AnEvent m Route -> Nut_ s m l p
  closureWithRouteEv currentRouteEv =
    D.div (bang $ D.Class := "flex flex-col")
      [ nav
      , D.div (bangCss "container")
          [ switcher routeToChild (currentRouteEv)
          ]
      ]
    where
    routeToChild :: Route -> Entity _ _ _ _
    routeToChild = case _ of
      Route.Landing -> Landing.nut
      _ -> text_ "Route not available"

    nav =
      D.div
        ( bangCss'
          [ fragCss "h-14 container mx-auto px-0 flex flex-row items-stretch justify-start mb-10"
          , fragCss "text-slate-400 font-semibold text-md"
          ]
        )
        [ menuItem Route.Landing "Home" ""
        , menuItem Route.PlayerList "Games" ""
        , menuItem Route.CreatePlayer "Players" ""
        , menuItem Route.PlaygroundDummy "Playground" ""
        , menuItem Route.PlaygroundFrp "Frp" ""
        ]
        where
        menuItem route text extraCss = D.a
          ( (bang $ D.Href := safeHref route)
            <|> itemCssEv
          )
          [ text_ text ]
          where
          baseCss = "hidden transition hover:text-teal-500 px-4 align-middle md:flex items-center"
          highlightCss currentRoute = if currentRoute == route then fragCss "text-teal-500" else ""
          itemCssEv = currentRouteEv <#> highlightCss >>> append baseCss >>> css