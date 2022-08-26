module Nuts.TopLevel where

import App.Env (Env)
import App.Navigation (routeChangeEvent)
import App.Route (Route)
import App.Route as Route
import Deku.Control (switcher, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import FRP.Event (ZoraEvent)
import Nuts.Debug as Debug
import Nuts.Game.GameNut as GameNut
import Nuts.Landing (nut) as Landing
import Nuts.Nav as Nav
import Nuts.Room.RoomNut as RoomNut
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (shareEv)

nut :: Env -> Nut
nut env =
  let
      closureWithRouteEv :: ZoraEvent Route -> Nut
      closureWithRouteEv currentRouteEv =
        D.div (bangCss "flex flex-col h-screen text-gray-100 bg-gray-700")
          [ Nav.nut currentRouteEv
          , switcher D.div (bangCss "h-full overflow-y-auto") routeToChild currentRouteEv
          ]
        where
        routeToChild :: Route -> Nut
        routeToChild = case _ of
          -- Route.Landing -> Debug.nut env
          Route.Landing -> Landing.nut
          Route.Room roomId -> RoomNut.nut env roomId
          Route.Game gameId -> GameNut.nut env gameId
          _ -> text_ "Route not available"
  in
  shareEv routeChangeEvent closureWithRouteEv