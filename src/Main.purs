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
  runInBody (switcher_ D.div identity (pure loadingNut <|> topLevelNutEv))

  where
  topLevelNutEv :: ∀ l p. ZoraEvent (Domable l p)
  topLevelNutEv = TopLevel.nut <$> envEv

  envEv :: ZoraEvent Env
  envEv = keepLatest (firebaseEv <#> createWithFb)
    where
    createWithFb fb = bus \push event -> {fb, myId: "7Mgc8HyJowTUe0gxLS3", appPush: push, appEvent: event}

  firebaseEv :: ZoraEvent FirebaseEnv
  firebaseEv = fromEvent $ fromAff $ startFirebase

  loadingNut :: ∀ l p. Domable l p
  loadingNut = text_ "Loading..."
