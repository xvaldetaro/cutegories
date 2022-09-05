module Nuts.Waiting.Controls where

import Prelude

import App.Env (AppEvent(..))
import App.Navigation (navigate)
import App.Route as Route
import Control.Alt ((<|>))
import Core.Room.RoomManager (leaveOrDeleteRoom)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Deku.Control (text)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState)
import Deku.Do as Doku
import Deku.Listeners (click)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Modal (btn, dialog)
import Nuts.Room.RoomEnv (RoomEnv)
import Nuts.Room.Scrollchat (copyUrlToClipboard)
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (ife)

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

    confirmDialogOpts =
      { description: "Are you sure you want to " <> dOrL <> " this room?"
      , buttons:
        [ btn { label: dOrL, action: doLeaveOrDeleteRoomEv, colorCss: Btn.redCss }
        , btn { label: "Cancel", action: (pure $ pushShowConfirm false) }
        ]
      }

    confirmDialog = dialog $ (ife (Just confirmDialogOpts) Nothing) <$> showConfirmEv

  D.div (bangCss "flex flex-col")
    [ confirmDialog
    , header
    ]

  where
  myId = (_.uid) $ unwrap self
  isAdmin = myId == roomId
  dOrL = if isAdmin then "Delete" else "Leave"