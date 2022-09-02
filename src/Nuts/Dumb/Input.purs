module Nuts.Dumb.Input where

import Prelude

import Control.Alt ((<|>))
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Attribute (Attribute, (:=))
import Deku.Core (Nut, Domable)
import Deku.DOM (Input_)
import Deku.DOM as D
import Deku.Do (useState')
import Deku.Do as Doku
import Deku.Listeners as DL
import FRP.Event (Event)
import Platform.Deku.Html (bangCss, css)

inputCss :: String
inputCss = css "placeholder:text-gray-400 text-white font-semibold bg-gray-900 rounded border-none"

inputText :: Event (Attribute Input_) -> Nut
inputText attrEv = D.input ((pure $ D.Xtype := "text") <|> attrEv) []

inputText' :: Event (Attribute Input_) -> Nut
inputText' attrEv = D.input (bangCss inputCss <|> (pure $ D.Xtype := "text") <|> attrEv) []

-- | Creates a text event Bus and already pushes the text into it automatically
-- | Use it like:
-- | myInput /\ myInputTextEv <- useInput \textEv -> (pure $ D.Placeholder := "asdf") <|> enterUp ...
-- useInput :: ∀ l p
--   . (Event String -> Domable)
--   -> ((Domable l p /\ Event String) -> Domable l p)
--   -> Domable l p
-- useInput getAttrsEv cont = Doku.do
--   pushText /\ textEv <- useState'
--   let attrsEv = (DL.textInput $ pure pushText) <|> (bangCss inputCss) <|> getAttrsEv textEv
--   cont $ (inputText attrsEv) /\ textEv

