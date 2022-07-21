module Dumb.Button where

import Prelude

import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

button :: ∀ props act. String -> act -> HH.HTML props act
button text act = HH.button
  [ HE.onClick $ const act
  , HP.classes
    [ ClassName "border border-slate-300 text-slate-700"
    , ClassName "w-32 rounded-md bg-slate-50 shadow-md cursor-pointer font-sans font-semibold "
    , ClassName "leading-loose px-3 py-1 transition select-none touch-manipulation align-middle "
    , ClassName "whitespace-nowrap break-words"
    , ClassName "hover:bg-slate-300 hover:no-underline disabled:bg-slate-50 disabled:border-slate-800"
    , ClassName "disabled:text-slate-400 disabled:cursor-default"
    , ClassName "active:bg-slate-200 active:shadow"
    , ClassName "focus:outline-1 focus:outline-transparent before:hidden"
    ]
  ]
  [ HH.text text ]