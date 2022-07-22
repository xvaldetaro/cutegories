module Components.Dumb.Icon where


import Prelude

import Data.Filterable (filter)
import Data.Newtype (unwrap)
import Data.String (joinWith, null)
import Halogen (ClassName)
import Halogen.HTML as HH
import Svg.Renderer.Halogen (icon)

type Icon
  = forall p r i. Array (HH.IProp r i) -> HH.HTML p i

classes :: forall r i. Array ClassName -> HH.IProp r i
classes =
  HH.attr (HH.AttrName "class")
    <<< joinWith " "
    <<< filter (not <<< null)
    <<< map unwrap

menu :: Icon
menu =
  icon
    """
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke="currentColor">
    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 6h16M4 12h16M4 18h16" />
  </svg>
  """
