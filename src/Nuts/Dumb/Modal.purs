module Nuts.Dumb.Modal where

import Prelude

import Deku.Core (Domable, Nut)
import Deku.DOM as D
import FRP.Event (Event)
import Platform.Deku.Html (bangCss, combineCss, css)
import Platform.Deku.Misc (ife)

modal :: ∀ l p. Event Boolean -> Domable l p -> Domable l p
modal showEv inner =
  D.div
    ( combineCss
        [ ife (css "flex") (css "hidden") <$> showEv
        , pure $ css "absolute top-20 left-0 right-0 justify-center"
        ]
    )
    [ inner ]

toast :: ∀ l p. Event Boolean -> Domable l p -> Domable l p
toast showEv inner = modal showEv $
  D.div (bangCss "rounded-full px-8 py-3 bg-blue-200 text-gray-800") [ inner ]
