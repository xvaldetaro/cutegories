module Main where

import Prelude

import App.Env (Env)
import App.Navigation (redirectToLandingIfInialRouteIsInvalid)
import Control.Alt ((<|>))
import Deku.Control (switcher_, text_)
import Deku.Core (Domable)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (ZoraEvent, bus, fromEvent, keepLatest)
import Nuts.TopLevel as TopLevel
import Paraglider.Operator.FromAff (fromAff)
import Platform.Firebase.Firebase (FirebaseEnv, startFirebase)

main :: Effect Unit
main = do
  redirectToLandingIfInialRouteIsInvalid
  runInBody TopLevel.nut
