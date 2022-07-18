module Dumb.Button where

import Prelude

import HTML.Utils (css)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

button :: ∀ props act. String -> act -> HH.HTML props act
button text act = HH.button
  [ HE.onClick $ const act
  , css "w-32 rounded bg-slate-300"
  ]
  [ HH.text text ]