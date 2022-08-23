module Nuts.Dumb.Btn where

import Prelude

import Control.Alt ((<|>))
import Deku.Control (text_)
import Deku.Core (class Korok, Domable)
import Deku.DOM as D
import Deku.Listeners (click)
import Effect (Effect)
import FRP.Event (AnEvent)
import Platform.Deku.Html (bangCss)

css :: String
css = "hover:bg-teal-300 bg-teal-200 shadow-sm rounded-md"

nut
  :: ∀ s m lock payload
  . Korok s m
  => String
  -> String
  -> AnEvent m (Effect Unit)
  -> Domable m lock payload
nut text extraCss onClick =
  D.button ((click onClick) <|> bangCss (css <> extraCss)) [text_ text]