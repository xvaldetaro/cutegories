module Platform.Deku.Html where

import Prelude

import Data.String (joinWith)
import Deku.Attribute (class Attr, Attribute, attr, (:=))
import Deku.DOM (Class, Id)
import Deku.DOM as D
import Deku.Listeners (keyUp)
import Effect (Effect)
import FRP.Event (ZoraEvent)
import Paraglider.Operator.Combine (combineFold')
import Web.UIEvent.KeyboardEvent as KeyboardEvent

-- / Creates a Class event from a String
bangCss :: ∀ e .Attr e Class String => String -> ZoraEvent (Attribute e)
bangCss s = pure (D.Class := s)

bangId :: ∀ e .Attr e Id String => String -> ZoraEvent (Attribute e)
bangId s = pure (D.Id := s)

-- / Creates a Class event from a concatenation of an Array String
bangCss' :: ∀ e .Attr e Class String => Array String -> ZoraEvent (Attribute e)
bangCss' xs = pure (D.Class := (joinWith " " xs))

-- / combines the emissions from multiple String events, concatenate them all and emit a Class event
-- / This is useful if you have both permanent and dynamic CSS classes in an element so you can
-- / create 2 separate events.
combineCss :: ∀ e. Attr e Class String => Array (ZoraEvent String) -> ZoraEvent (Attribute e)
combineCss stringEvents = attr D.Class <$> combineFold' stringEvents

-- Used to prefix a CSS string with "Css" so that Tailwind's VSCode extension can detect it and
-- launch intellisense
css :: String -> String
css s = " " <> s <> " "

enterUp :: ∀ element. ZoraEvent (Effect Unit) -> ZoraEvent (Attribute element)
enterUp effEv = keyUp $ filterEnter <$> effEv
  where
  filterEnter eff = \kbEvent -> if KeyboardEvent.code kbEvent == "Enter" then eff else pure unit