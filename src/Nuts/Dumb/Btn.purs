module Nuts.Dumb.Btn where

import Prelude

import Control.Alt ((<|>))
import Data.String (joinWith)
import Deku.Control (text_)
import Deku.Core (class Korok, Domable)
import Deku.DOM as D
import Deku.Listeners (click)
import Effect (Effect)
import FRP.Event (AnEvent)
import Platform.Deku.Html (bangCss, css)

teal
  :: ∀ s m lock payload
  . Korok s m
  => String
  -> String
  -> AnEvent m (Effect Unit)
  -> Domable m lock payload
teal text extraCss onClick =
  D.button ((click onClick) <|> bangCss (baseCss <> tealCss <> extraCss)) [text_ text]

gray
  :: ∀ s m lock payload
  . Korok s m
  => String
  -> String
  -> AnEvent m (Effect Unit)
  -> Domable m lock payload
gray text extraCss onClick =
  D.button ((click onClick) <|> bangCss (baseCss <> grayCss <> extraCss)) [text_ text]

tealCss :: String
tealCss = css "bg-teal-600 hover:bg-teal-500 active:bg-green-200"

grayCss :: String
grayCss = css "bg-gray-550 hover:bg-gray-400 active:bg-gray-400"

baseCss :: String
baseCss = joinWith ""
  [ css "font-semibold px-3 py-1 text-white text-center"
  , css "rounded-md shadow-md cursor-pointer whitespace-nowrap"
  , css "leading-loose transition select-none touch-manipulation align-middle hover:no-underline "
  , css "active:shadow focus:outline-1 focus:outline-transparent before:hidden break-words "
  ]