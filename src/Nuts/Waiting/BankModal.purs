module Nuts.Waiting.BankModal where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Core.Room.BankManager (getCombinedList)
import Core.Room.FormsPersistManager (getFormsPersist, saveFormsPersist)
import Core.Room.GameManager (setAllowNonAdmins, startGame)
import Data.Int (floor)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (attr, (:=))
import Deku.Control (text, text_)
import Deku.Core (Domable)
import Deku.DOM as D
import Deku.Do (useState)
import Deku.Do as Doku
import Deku.Listeners (checkbox, click)
import Deku.Listeners as DL
import Effect (Effect)
import Effect.Aff (launchAff_)
import FRP.Event.VBus (V)
import Models.Models (FormsPersistRow)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Input (inputCss, inputText')
import Nuts.Room.RoomEnv (RoomEnv)
import Paraglider.Operator.Combine (combineLatest3)
import Paraglider.Operator.FromAff (fromAff)
import Paraglider.Operator.Take (take)
import Platform.Deku.Html (bangCss, bangPlaceholder, css)
import Platform.Deku.Misc (cleanFbAff, dynDiffOrdered, ife, useCleanFbEvent)
import Platform.Deku.VBusHelp (vbussedFrom)
import Platform.Firebase.Auth (uid)
import Type.Proxy (Proxy(..))


nut :: ∀ l p. RoomEnv -> Effect Unit -> (String -> Effect Unit) -> Domable l p
nut { env: env@{fb, self, errPush}, playersEv, roomId, gameEv} pushDismiss pushCategory = Doku.do
  categoriesEv <- useCleanFbEvent env $ fromAff $ getCombinedList env

  let
    render ctg =
      D.li
        ( (bangCss "font-semibold mx-3 py-1 mb-1 rounded-md bg-gray-700")
          <|> click (pure $ pushCategory ctg)
        )
        [ D.i (bangCss "mx-3 ion-close-circled text-red-400") []
        , text_ ctg
        ]
  D.div
    (bangCss "w-full h-full bg-gray-500 flex flex-col p-3 mx-14 mb-14 overflow-y-scroll text-sm")
    [ D.div (bangCss "w-full")
        [ D.i (bangCss "mx-3 ion-close-round" <|> click (pure $ pushDismiss)) []
        ]
    , dynDiffOrdered D.ol (bangCss "flex flex-col mt-4") identity render categoriesEv
    ]