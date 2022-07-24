module Dumb.Break where

import Halogen.HTML as HH
import Platform.Html.CssUtils (css)

break :: ∀ props act. HH.HTML props act
break =
  HH.div [css "flex flex-row w-full items-center justify-between my-4"]
    [ HH.div [ css "h-0 w-full border-t border-slate-300"] []
    , HH.div [ css "text-slate-400 mx-4"] [ HH.text "Or" ]
    , HH.div [ css "h-0 w-full border-t border-slate-300"] []
    ]