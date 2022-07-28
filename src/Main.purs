module Main where

import Prelude

import Components.Bootstrapper as Bootstrapper
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = launchAff_ do
  body <- HA.awaitBody
  void $ runUI Bootstrapper.component unit body
