module Nuts.Dumb.Input where

import Prelude

import Control.Alt ((<|>))
import Deku.Attribute (Attribute, (:=))
import Deku.Core (Nut)
import Deku.DOM (Input_)
import Deku.DOM as D
import FRP.Event (Event)
import Platform.Deku.Html (bangCss, css)

inputCss :: String
inputCss = css "placeholder:text-gray-600 text-sm font-medium bg-gray-900 rounded border-none"

inputText :: Event (Attribute Input_) -> Nut
inputText attrEv = D.input ((pure $ D.Xtype := "text") <|> attrEv) []

inputText' :: Event (Attribute Input_) -> Nut
inputText' attrEv = D.input (bangCss inputCss <|> (pure $ D.Xtype := "text") <|> attrEv) []