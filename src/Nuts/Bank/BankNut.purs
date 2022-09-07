module Nuts.Bank.BankNut where

import Prelude

import App.Env (Env)
import App.Navigation (navigate)
import App.Route as Route
import Control.Alt ((<|>))
import Core.Room.BankManager (addCategory, getCombinedList)
import Data.Array (singleton, sort)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (attr)
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState, useState')
import Deku.Do as Doku
import Deku.Listeners (click)
import Deku.Listeners as DL
import Effect.Aff (launchAff_)
import FRP.Event (folded)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Input (inputCss, inputText')
import Paraglider.Operator.Combine (combineLatest)
import Paraglider.Operator.FromAff (fromAff)
import Platform.Deku.Html (bangCss, bangPlaceholder, css, enterUp)
import Platform.Deku.Misc (cleanFbAff, dynDiffOrdered, useCleanFbEvent)

nut :: Env -> Nut
nut env = Doku.do
  categoriesEv <- useCleanFbEvent env $ fromAff $ getCombinedList env
  pushAdded /\ addedEv' <- useState []
  pushText /\ textEv <- useState'

  let
    addedEv = folded addedEv'

    doAddCategory ctg = do
      pushAdded [ctg]
      launchAff_ $ cleanFbAff env $ addCategory env ctg

    allEv = combineLatest combine categoriesEv addedEv
      where
      combine a b = sort $ a <> b

    inputField = D.label (bangCss "ml-3 flex items-center font-medium")
      [ D.span (bangCss "mr-2") [text_ "Category"]
      , D.div (bangCss "flex-grow mr-3")
          [ inputText'
            ( (bangCss $ (css "w-full") <> inputCss)
              <|> (bangPlaceholder "Countries In Europe")
              <|> (attr D.Value <<< const "" <$> addedEv')
              <|> (DL.textInput $ pure $ pushText)
              <|> enterUp (textEv <#> doAddCategory)
            )
          , D.i (bangCss "ion-close-circled -ml-5" <|> (click $ pure $ pushAdded [])) []
          ]
      ]

    render ctg =
      D.li
        (bangCss "text-lg mx-3 mb-1 rounded-md bg-gray-700")
        [ D.i (bangCss "mx-3 ion-close-circled text-red-400") []
        , text_ ctg
        ]

    leaveBtn = D.i ((click $ pure $ navigate Route.Landing) <|> bangCss ("ion-checkmark text-xl")) []

    header = D.div (bangCss "px-3 py-2 flex items-center justify-items-stretch w-full bg-gray-700")
      [ D.i (bangCss "ion-close-round text-xl hidden") []
      , D.span (bangCss "flex-grow w-full flex-col text-center text-lg text-white")
          [ text_ "Category Bank" ]
      , leaveBtn
      ]

  D.div (bangCss ("flex flex-col bg-gray-800 h-full w-full"))
    [ header
    , D.div (bangCss "flex mt-3")
        [ inputField
        , Btn.gray "Add" "" (textEv <#> doAddCategory)
        ]
    , dynDiffOrdered D.ol (bangCss "flex flex-col mt-4") identity render allEv
    ]