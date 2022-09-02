module Main where

import Prelude

import App.Navigation (redirectToLandingIfInialRouteIsInvalid)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Nuts.TopLevel as TopLevel

main :: Effect Unit
main = do
  redirectToLandingIfInialRouteIsInvalid
  runInBody TopLevel.nut
