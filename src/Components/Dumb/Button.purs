module Dumb.Button where

import Prelude

import Components.Dumb.Styles (buttonCss, buttonCss')
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

button :: ∀ props act. String -> act -> HH.HTML props act
button text act = HH.button
  [ HE.onClick $ const act
  , buttonCss'
  ]
  [ HH.text text ]