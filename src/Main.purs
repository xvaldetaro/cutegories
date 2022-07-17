module Main where

import Prelude

import Components.Bootstrapper as Bootstrapper
import Effect (Effect)
import Effect.Aff (launchAff_)
import Firebase.Firebase (startFirebase)
import Halogen.Aff as HA
import Halogen.Store.Monad (runStoreT)
import Halogen.VDom.Driver (runUI)
import Store.MyStore as MyStore

main :: Effect Unit
main = launchAff_ do
  body <- HA.awaitBody
  void $ runUI Bootstrapper.component unit body
