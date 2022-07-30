module Platform.Deku.Html where

import Prelude

import Data.Foldable (for_)
import Deku.Attribute (class Attr, Attribute, cb, (:=))
import Deku.DOM (Class)
import Deku.DOM as D
import Effect (Effect)
import FRP.Event (AnEvent, bang)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value)

ezCss :: ∀ e m. Applicative m => Attr e Class String => String -> AnEvent m (Attribute e)
ezCss s = bang $ D.Class := s

ezClick :: ∀ e m. Applicative m => Effect Unit -> AnEvent m (Attribute e)
ezClick effect = bang $ D.OnClick := cb (const effect)

ezInput :: ∀ e m. Applicative m => (String -> Effect Unit) -> AnEvent m (Attribute e)
ezInput userCb = bang $ D.OnInput := cb \webEvent ->
  let mbTargetInputElement = target webEvent >>= fromEventTarget in
  for_ mbTargetInputElement (value >=> userCb)
