module Nuts.Dumb.Modal where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import ConvertableOptions (class Defaults, defaults)
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Deku.Control (switcher, switcher_, text_)
import Deku.Core (Domable, Nut)
import Deku.DOM as D
import Deku.Listeners (click)
import Effect (Effect)
import FRP.Event (Event)
import Nuts.Dumb.Btn (grayCss)
import Nuts.Dumb.Btn as Btn
import Platform.Deku.Html (bangCss, combineCss, css)
import Platform.Deku.Misc (ife)

modal :: ∀ l p. Event Boolean -> Domable l p -> Domable l p
modal showEv inner =
  D.div
    ( combineCss
        [ ife (css "flex") (css "hidden") <$> showEv
        , pure $ css "absolute top-20 left-0 right-0 justify-center"
        ]
    )
    [ inner ]

toast :: ∀ l p. Event Boolean -> Domable l p -> Domable l p
toast showEv inner = modal showEv $
  D.div (bangCss "rounded-full px-8 py-3 bg-blue-200 text-gray-800") [ inner ]

data BtnColor = BtnRed | BtnGreen | BtnGray
type BtnOpts =
  { label :: String
  , action :: Event (Effect Unit)
  , colorCss :: String
  }

defaultBtnOpts :: BtnOpts
defaultBtnOpts = { label: "", action: empty, colorCss: Btn.grayCss }

btn :: ∀ opts. Defaults BtnOpts opts BtnOpts => opts -> BtnOpts
btn = defaults defaultBtnOpts

type DialogOpts =
  { description :: String
  , buttons :: Array BtnOpts
  , showHideEv :: Maybe (Event Boolean)
  }

defaultDialogOpts :: DialogOpts
defaultDialogOpts =
  { description: ""
  , buttons: []
  , showHideEv: Nothing
  }

dialog :: ∀ opts. Defaults DialogOpts opts DialogOpts => Event (Maybe opts) -> Nut
dialog optsEv = switcher_ D.div go (map (defaults defaultDialogOpts) <$> optsEv)
  where
  go Nothing = D.div (bangCss "hidden") []
  go (Just { description, buttons, showHideEv }) = modal (fromMaybe (pure true) showHideEv) $ D.div
    (bangCss "bg-gray-200 rounded-md flex flex-col p-3 m-14")
    [ D.span (bangCss "text-gray-900 mb-3 ")
        [ text_ description ]
    , D.div (bangCss "flex justify-items-stretch w-full gap-2")
        (renderBtn <$> buttons)
    ]

  renderBtn {label, action, colorCss} =
    D.button ((click action) <|> bangCss (Btn.baseCss <> colorCss <> css "flex-grow")) [text_ label]