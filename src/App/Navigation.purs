module App.Navigation where

import Prelude

import App.Route (Route, routeCodec)
import App.Route as Route
import Data.Either (hush)
import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Class.Console (log)
import FRP.Event (Event, makeEvent)
import Routing.Duplex (parse, print)
import Routing.Duplex as RD
import Routing.Hash (getHash, matchesWith, setHash)

navigate :: Route -> Effect Unit
navigate = setHash <<< print Route.routeCodec

redirectToLandingIfInialRouteIsInvalid :: Effect Unit
redirectToLandingIfInialRouteIsInvalid = do
  initialRoute <- hush <<< (RD.parse routeCodec) <$> getHash
  when (isNothing initialRoute) do
    log "Got invalid Route. Navigating to Landing."
    navigate Route.Landing

routeChangeEvent :: Event Route
routeChangeEvent = makeEvent \k -> do
  matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      log $ "Changing route from " <> show old <> " to " <> show new
      k new