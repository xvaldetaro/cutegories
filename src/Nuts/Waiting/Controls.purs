module Nuts.Waiting.Controls where

import Prelude

import App.Env (AppEvent(..))
import App.Navigation (navigate)
import App.Route as Route
import Control.Alt ((<|>))
import Core.Room.RoomManager (leaveOrDeleteRoom)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState, useState')
import Deku.Do as Doku
import Deku.Listeners (click)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Modal (modal)
import Nuts.Room.RoomEnv (RoomEnv)
import Nuts.Room.Scrollchat (copyUrlToClipboard)
import Nuts.Waiting.GameDetails as GameDetails
import Platform.Deku.Html (bangCss, css)

nut :: RoomEnv -> Nut
nut roomEnv@{ env: { fb, self, errPush, appPush }, playersEv, roomId, roomEv } = Doku.do
  pushShowConfirm /\ showConfirmEv <- useState false

  let
    doLeaveOrDeleteRoom = launchAff_ do
      eiResult <- leaveOrDeleteRoom fb roomId myId
      liftEffect $ case eiResult of
        Left e -> errPush e
        Right _ -> navigate $ Route.Landing

    doLeaveOrDeleteRoomEv = pure doLeaveOrDeleteRoom
    doCopyToClip = copyUrlToClipboard *> appPush (ShowToast "Copied link!" 2000)

    leaveOrDeleteBtn = D.i
      ( (click $ pure $ pushShowConfirm true)
          <|> bangCss "text-red-600 ion-close-round text-xl"
      )
      []

    header = D.div (bangCss "px-3 py-2 flex items-center justify-items-stretch w-full bg-gray-700")
      [ leaveOrDeleteBtn
      , D.span (bangCss "flex-grow w-full flex-col text-center text-lg text-white")
          [ text $ (_.title) <$> roomEv ]
      , D.i ((click $ pure doCopyToClip) <|> bangCss "text-gray-300 ion-forward text-xl") []
      ]

    confirmModal = modal showConfirmEv $ D.div
      (bangCss "bg-gray-200 rounded-md flex flex-col p-3 m-14")
      [ D.span (bangCss "text-gray-900 mb-3 ")
          [ text_ $ "Are you sure you want to " <> dOrL <> " this room?" ]
      , D.div (bangCss "flex justify-items-stretch w-full")
          [ Btn.red dOrL "flex-grow mr-1" (doLeaveOrDeleteRoomEv)
          , Btn.gray "Cancel" "flex-grow ml-1" (pure $ pushShowConfirm false)
          ]
      ]

  D.div (bangCss "flex flex-col")
    [ confirmModal
    , header
    ]

  where
  myId = (_.uid) $ unwrap self
  isAdmin = myId == roomId
  dOrL = if isAdmin then "Delete" else "Leave"