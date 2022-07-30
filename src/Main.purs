module Main where

import Prelude

import App.Env (Env)
import Control.Alt ((<|>))
import Control.Monad.Reader (runReader)
import Deku.Control (text_)
import Deku.Core (Domable, Nut)
import Deku.Interpret (FFIDOMSnapshot)
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Event (Event, bang, bus, keepLatest)
import HyruleRx (fromAff)
import Nuts.TopLevel as TopLevel
import Platform.Firebase.Firebase (FirebaseEnv, startFirebase)

main :: Effect Unit
main =
  runInBody1 (topLevelNutEv <|> bang loadingNut)

  where
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
