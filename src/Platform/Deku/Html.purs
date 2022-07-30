module Platform.Deku.Html where

import Prelude

import Data.Foldable (for_)
import Data.String (joinWith)
import Deku.Attribute (class Attr, Attribute, cb, (:=))
import Deku.DOM (Class)
import Deku.DOM as D
import Effect (Effect)
import FRP.Event (AnEvent, bang)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value)

css :: ∀ e. Attr e Class String => String -> Attribute e
css s = D.Class := s

css' :: ∀ e. Attr e Class String => Array String -> Attribute e
css' s = D.Class := (joinWith " " s)

bangCss :: ∀ e m. Applicative m => Attr e Class String => String -> AnEvent m (Attribute e)
bangCss s = bang $ css s

bangCss' :: ∀ e m. Applicative m => Attr e Class String => Array String -> AnEvent m (Attribute e)
bangCss' s = bang $ css' s

-- Used to prefix a CSS string with "Css" so that Tailwind's VSCode extension can detect it and
-- launch intellisense
fragCss :: String -> String
fragCss = identity

bangClick :: ∀ e m. Applicative m => Effect Unit -> AnEvent m (Attribute e)
bangClick effect = bang $ D.OnClick := cb (const effect)

bangInput :: ∀ e m. Applicative m => (String -> Effect Unit) -> AnEvent m (Attribute e)
bangInput userCb = bang $ D.OnInput := cb \webEvent ->
  let mbTargetInputElement = target webEvent >>= fromEventTarget in
  for_ mbTargetInputElement (value >=> userCb)
