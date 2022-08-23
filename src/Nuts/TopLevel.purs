module Nuts.TopLevel where

import Prelude

import App.Env (AppNut_, Nut_)
import App.Navigation (routeChangeEvent)
import App.Route (Route)
import App.Route as Route
import Bolson.Core (Entity)
import Control.Monad.Reader (ask)
import Deku.Control (switcher, switcher_, text_)
import Deku.DOM as D
import FRP.Event (AnEvent)
import Nuts.Landing (nut) as Landing
import Nuts.Nav as Nav
import Nuts.RoomNut as RoomNut
import Paraglider.Operator.Replay (replayRefCount)
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (usingEffect)

nut :: ∀ s m l p. AppNut_ s m l p
nut = do
  env <- ask
  let
      closureWithRouteEv :: AnEvent m Route -> Nut_ s m l p
      closureWithRouteEv currentRouteEv =
        D.div (bangCss "flex flex-col h-screen")
          [ Nav.nut currentRouteEv
          , switcher D.div (bangCss "h-full") routeToChild currentRouteEv
          ]
        where
        routeToChild :: Route -> Entity _ _ _ _
        routeToChild = case _ of
          Route.Landing -> Landing.nut
          Route.Room roomId -> RoomNut.nut env roomId
          _ -> text_ "Route not available"
  pure $ usingEffect (replayRefCount routeChangeEvent) \currentRouteEv ->
    closureWithRouteEv currentRouteEv