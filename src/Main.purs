module Main where

import Prelude

import App.Env (Env)
import App.Navigation (redirectToLandingIfInialRouteIsInvalid)
import Control.Alt ((<|>))
import Control.Monad.Reader (runReader)
import Deku.Control (switcher, text_)
import Deku.Core (Domable, Nut)
import Deku.Interpret (FFIDOMSnapshot)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (Event, bang, bus, fromEvent, keepLatest)
import Nuts.TopLevel as TopLevel
import Paraglider.AffBridge (fromAff)
import Platform.Firebase.Firebase (FirebaseEnv, startFirebase)

main :: Effect Unit
main = do
  redirectToLandingIfInialRouteIsInvalid
  runInBody switcherNut

  where
  switcherNut :: ∀ l. Domable Effect l (FFIDOMSnapshot -> Effect Unit)
  switcherNut = switcher identity (fromEvent (topLevelNutEv <|> bang loadingNut))

  topLevelNutEv :: ∀ l. Event (Domable Effect l (FFIDOMSnapshot -> Effect Unit))
  topLevelNutEv = runReader TopLevel.nut <$> envEv

  envEv :: Event (Env Effect)
  envEv = keepLatest (firebaseEv <#> createWithFb)
    where
    createWithFb fb = bus \push event -> {fb, appPush: push, appEvent: event}

  firebaseEv :: Event FirebaseEnv
  firebaseEv = fromAff startFirebase

  loadingNut :: Nut
  loadingNut = text_ "Loading..."
