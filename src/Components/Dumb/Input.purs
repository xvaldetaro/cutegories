module Dumb.Input where

import Prelude

import Components.Dumb.Styles (buttonCss, buttonCss')
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Platform.Html.CssUtils (css)

input :: ∀ props act. String -> String -> (String -> act) -> HH.HTML props act
input label id cb =
  HH.fieldset [ css "flex flex-col w-full font-semibold text-slate-500 text-md mb-4"]
    [ HH.label [HP.for id, css "mb-2" ] [ HH.text label ]
    , HH.input
      [HP.id id
      , HE.onValueInput cb
      , HP.placeholder "12345"
      , css "rounded-lg border border-slate-600 px-4 py-1"
      ]
    ]