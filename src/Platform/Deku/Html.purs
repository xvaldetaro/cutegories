module Platform.Deku.Html where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.String (joinWith)
import Deku.Attribute (class Attr, Attribute, attr, (:=))
import Deku.Core (class Korok)
import Deku.DOM (Class)
import Deku.DOM as D
import Deku.Listeners (keyUp)
import Effect (Effect)
import FRP.Event (AnEvent)
import Paraglider.Operator.Combine (combineFold')
import Web.UIEvent.KeyboardEvent as KeyboardEvent

-- / Creates a Class event from a String
bangCss :: ∀ e s m. Korok s m => Attr e Class String => String -> AnEvent m (Attribute e)
bangCss s = pure (D.Class := s)

-- / Creates a Class event from a concatenation of an Array String
bangCss' :: ∀ e s m. Korok s m => Attr e Class String => Array String -> AnEvent m (Attribute e)
bangCss' xs = pure (D.Class := (joinWith " " xs))

-- / combines the emissions from multiple String events, concatenate them all and emit a Class event
-- / This is useful if you have both permanent and dynamic CSS classes in an element so you can
-- / create 2 separate events.
combineCss
  :: ∀ e s m
   . MonadST s m
  => Attr e Class String
  => Array (AnEvent m String)
  -> AnEvent m (Attribute e)
combineCss stringEvents = attr D.Class <$> combineFold' stringEvents

-- Used to prefix a CSS string with "Css" so that Tailwind's VSCode extension can detect it and
-- launch intellisense
css :: String -> String
css s = " " <> s <> " "

enterUp
  :: forall s m eleemnt
   . MonadST s m
  => AnEvent m (Effect Unit)
  -> AnEvent m (Attribute eleemnt)
enterUp effEv = keyUp $ filterEnter <$> effEv
  where
  filterEnter eff = \kbEvent -> if KeyboardEvent.code kbEvent == "Enter" then eff else pure unit