module Main where

import Prelude

import App.Env (Env)
import App.Navigation (redirectToLandingIfInialRouteIsInvalid)
import Control.Alt ((<|>))
import Control.Monad.Reader (runReader)
import Core.Room.RoomManager (mockChat)
import Deku.Control (switcher_, text_)
import Deku.Core (Domable, Nut)
import Deku.DOM as D
import Deku.Interpret (FFIDOMSnapshot)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (Event, bus, fromEvent, keepLatest)
import Nuts.TopLevel as TopLevel
import Paraglider.Operator.FromAff (fromAff)
import Platform.Firebase.Firebase (FirebaseEnv, startFirebase)

main :: Effect Unit
main = do
  redirectToLandingIfInialRouteIsInvalid
  runInBody switcherNut

  where
  switcherNut :: ∀ l. Domable Effect l (FFIDOMSnapshot -> Effect Unit)
  switcherNut = switcher_ D.div identity (fromEvent (pure loadingNut <|> topLevelNutEv))

  topLevelNutEv :: ∀ l. Event (Domable Effect l (FFIDOMSnapshot -> Effect Unit))
  topLevelNutEv = runReader TopLevel.nut <$> envEv

  envEv :: Event (Env Effect)
  envEv = keepLatest (firebaseEv <#> createWithFb)
    where
    createWithFb fb = bus \push event -> {fb, appPush: push, appEvent: event}

  firebaseEv :: Event FirebaseEnv
  firebaseEv = fromAff $ startFirebase mockChat

  loadingNut :: Nut
  loadingNut = text_ "Loading..."
