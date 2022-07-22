module Dumb.VerticalListClickable where

import Prelude

import Platform.Html.CssUtils (css)
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Model r = { text :: String | r }

verticalListClickable :: ∀ props act r. Array (Model r) -> ((Model r) -> act) -> HH.HTML props act
verticalListClickable models callback = HH.div
  [ css "w-64 flex-col"]
  (renderItem <$> models)
  where
  renderItem model@{text} = HH.div
    [ HP.classes
      [ ClassName "text-slate-800 cursor-pointer font-sans leading-normal"
      , ClassName "px-4 py-2 border first:rounded-t-md last:rounded-b-md first:border-b-0"
      , ClassName "last:border-t-0 border-slate-200"
      , ClassName "hover:bg-slate-50 hover:no-underline transition active:bg-slate-300"
      ]
    , HE.onClick \_ -> callback model
    ]
    [ HH.text text ]