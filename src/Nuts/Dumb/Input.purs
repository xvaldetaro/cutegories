module Nuts.Dumb.Input where

import Prelude

import Control.Alt ((<|>))
import Deku.Attribute (Attribute, (:=))
import Deku.Core (Nut)
import Deku.DOM (Input_)
import Deku.DOM as D
import FRP.Event (ZoraEvent)
import Platform.Deku.Html (css)

inputCss :: String
inputCss = css "placeholder:text-gray-400 text-white font-semibold bg-gray-900 rounded border-none"

inputText
  :: forall s m lock payload
   . ZoraEvent (Attribute Input_)
  -> Nut
inputText attrEv = D.input ((pure $ D.Xtype := "text") <|> attrEv) []