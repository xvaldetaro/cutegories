module Platform.Deku.Html where

import Prelude

import Control.Alt (alt)
import Data.Foldable (for_)
import Data.String (joinWith)
import Deku.Attribute (class Attr, Attribute, attr, cb, (:=))
import Deku.DOM (Class, Id, Placeholder, Value)
import Deku.DOM as D
import Deku.Listeners (keyUp)
import Effect (Effect)
import FRP.Event (Event)
import Paraglider.Operator.Combine (combineFold')
import Web.DOM (Element)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (checked, fromEventTarget, value, valueAsNumber)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent


-- / Creates a Class event from a String
bangCss :: ∀ e .Attr e Class String => String -> Event (Attribute e)
bangCss s = pure (D.Class := s)

bangId :: ∀ e .Attr e Id String => String -> Event (Attribute e)
bangId s = pure (D.Id := s)

bangPlaceholder :: ∀ e .Attr e Placeholder String => String -> Event (Attribute e)
bangPlaceholder s = pure (D.Placeholder := s)

bangValue :: ∀ e .Attr e Value String => String -> Event (Attribute e)
bangValue s = pure (D.Value := s)

-- / Creates a Class event from a concatenation of an Array String
bangCss' :: ∀ e .Attr e Class String => Array String -> Event (Attribute e)
bangCss' xs = pure (D.Class := (joinWith " " xs))

-- / combines the emissions from multiple String events, concatenate them all and emit a Class event
-- / This is useful if you have both permanent and dynamic CSS classes in an element so you can
-- / create 2 separate events.
combineCss :: ∀ e. Attr e Class String => Array (Event String) -> Event (Attribute e)
combineCss stringEvents = attr D.Class <$> combineFold' stringEvents

-- Used to prefix a CSS string with "Css" so that Tailwind's VSCode extension can detect it and
-- launch intellisense
css :: String -> String
css s = " " <> s <> " "

enterUp :: ∀ element. Event (Effect Unit) -> Event (Attribute element)
enterUp effEv = keyUp $ filterEnter <$> effEv
  where
  filterEnter eff = \kbEvent -> if KeyboardEvent.code kbEvent == "Enter" then eff else pure unit

hideIf :: Boolean -> String
hideIf x = if x then css "hidden" else ""

showIf :: Boolean -> String
showIf x = if x then "" else css "hidden"

-- checkboxListener
--   :: Event (Boolean -> Effect Unit)
--   -> Event (Attribute D.Input_)
-- checkboxListener = alt (pure $ D.Xtype := "checkbox") <<< map
--   ( \push ->
--       D.OnInput := cb \e -> for_
--         (target e >>= fromEventTarget)
--         (checked >=> push)
--   )