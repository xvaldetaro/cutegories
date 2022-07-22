module Components.Dumb.Styles where

import Prelude

import Halogen.HTML (IProp)
import Halogen.HTML.Properties as HP
import Web.HTML.Common (ClassName(..))

cardCss :: ∀ r i. String -> IProp (class :: String | r) i
cardCss s = HP.class_ $ ClassName $ "shadow container mx-auto rounded-lg px-6 py-4 " <> s

cardCss' :: ∀ r i. IProp (class :: String | r) i
cardCss' = cardCss ""

buttonCss' :: ∀ r i. IProp (class :: String | r) i
buttonCss' = buttonCss ""

buttonCss :: ∀ r i. String -> IProp (class :: String | r) i
buttonCss s = HP.classes
  [ ClassName "text-white font-sans"
  , ClassName "rounded-md bg-teal-500 shadow-md cursor-pointer font-sans font-semibold "
  , ClassName "leading-loose px-4 py-1 transition select-none touch-manipulation align-middle "
  , ClassName "whitespace-nowrap break-words"
  , ClassName "hover:bg-teal-400 hover:no-underline disabled:bg-slate-50 disabled:border-slate-800"
  , ClassName "disabled:text-slate-400 disabled:cursor-default"
  , ClassName "active:bg-green-200 active:shadow"
  , ClassName $ "focus:outline-1 focus:outline-transparent before:hidden " <> s
  ]
