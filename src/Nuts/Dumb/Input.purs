module Nuts.Dumb.Input where

import Prelude

import Data.Foldable (oneOf)
import Deku.Attribute (Attribute, (:=))
import Deku.Control (text)
import Deku.Core (class Korok, Domable)
import Deku.DOM (Input_)
import Deku.DOM as D
import FRP.Event (AnEvent)
import Platform.Deku.Html (bangCss)

inputCss :: String
inputCss = "rounded-lg border border-slate-600 px-4 py-1"
nut
  :: ∀ s m lock payload
  . Korok s m
  => {forId :: String, labelText :: AnEvent m String, inputAttrs :: AnEvent m (Attribute Input_) }
  -> Domable m lock payload
nut { forId, labelText, inputAttrs } =
  D.fieldset (bangCss "flex flex-col w-full font-semibold text-slate-500 text-md mb-4")
    [ D.label
      (oneOf
        [ pure $ D.For := forId
        , bangCss "mb-2"
        ]
      )
      [ text labelText ]
    , D.input
      (oneOf
        [ pure $ D.Id := forId
        , bangCss inputCss
        , inputAttrs
        ]
      )
      []
    ]