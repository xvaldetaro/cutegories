module Nuts.Landing where

import Prelude

import Control.Monad.Reader (ask)
import Deku.Attribute ((:=))
import Deku.Control (text)
import Deku.Core (class Korok, Domable, Nut, bussed)
import Deku.DOM as D
import FRP.Event (bang)

nut :: Nut
nut = bussed \push event ->
    D.div (bang $ D.Class := "flex flex-col")
      [ D.div ( bang $ D.Class := "text-red-700") [ text $ bang $ "Landing page: " <> show 12 ]
      ]