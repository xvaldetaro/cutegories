module Nuts.Game.TimerBar where

import Prelude

import Control.Alt ((<|>))
import Data.Int (floor)
import Data.Time.Duration (Seconds(..))
import Deku.Attribute (attr)
import Deku.Attributes (style)
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import FRP.Event (Event)
import Platform.Deku.Html (bangCss)

nut :: Event Int -> Event Seconds -> Nut
nut progress seconds =
  D.div (bangCss "mx-3 flex flex-col")
    [ D.div
        (bangCss "bg-gray-600 rounded-full h-3")
        [ D.div
            ( (bangCss "bg-blue-300 h-3 rounded-full")
              <|> (attr D.Style <<< (\p -> "width:" <> show p <> "%") <$> progress)
            ) []
        ]
    , D.span (bangCss "text-blue-300 text-center")
      [ text $ (\(Seconds s) -> show (floor s) <> "s") <$> seconds
      ]
    ]
