module Platform.Html.CssUtils where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

css :: forall r i. String -> HH.IProp (class :: String | r) i
css = HP.class_ <<< HH.ClassName


cx :: ClassName -> Boolean -> ClassName
cx c cond = if cond then c else ClassName ""