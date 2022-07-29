module Main where

import Prelude

import Components.Bootstrapper as Bootstrapper
import Deku.Control (text_)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Nuts.TopLevel as TopLevel

-- main :: Effect Unit
-- main = launchAff_ do
--   body <- HA.awaitBody
--   void $ runUI Bootstrapper.component unit body

main :: Effect Unit
main = runInBody (TopLevel.topLevel { param: "param1"})