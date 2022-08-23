module Nuts.CSS.CSSSnippets where

import Data.String (joinWith)
import Platform.Deku.Html (css)

cardCss :: String
cardCss =  "shadow container mx-auto rounded-lg px-6 py-4 "

buttonCss :: String
buttonCss = joinWith " "
  [ css "text-white font-sans"
  , css "rounded-md bg-teal-500 shadow-md cursor-pointer font-sans font-semibold "
  , css "leading-loose px-4 py-1 transition select-none touch-manipulation align-middle "
  , css "whitespace-nowrap break-words"
  , css "hover:bg-teal-400 hover:no-underline disabled:bg-slate-50 disabled:border-slate-800"
  , css "disabled:text-slate-400 disabled:cursor-default"
  , css "active:bg-green-200 active:shadow"
  , css "focus:outline-1 focus:outline-transparent before:hidden "
  ]
