module Nuts.Room.RoomLeftBar where

import Prelude

import App.Navigation (navigate)
import App.Route as Route
import Control.Alt ((<|>))
import Core.Room.GameManager (changeGameState)
import Core.Room.RoomManager (leaveOrDeleteRoom)
import Data.Array (length)
import Data.Either (Either(..), either)
import Data.Newtype (unwrap)
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState')
import Deku.Do as Doku
import Deku.Listeners (click)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Models.Models (GameState(..), Player)
import Nuts.Dumb.Btn as Btn
import Nuts.Room.GameDetails as GameDetails
import Nuts.Room.RoomEnv (RoomEnv)
import Platform.Deku.Html (bangCss, combineCss, css)

nut :: RoomEnv -> Nut
nut roomEnv@{ env: {fb, self, errPush}, playersEv, roomId, roomEv } = Doku.do
  let
    doLeaveOrDeleteRoom players = launchAff_ do
      eiResult <- leaveOrDeleteRoom fb roomId myId
      liftEffect $ case eiResult of
        Left e -> errPush e
        Right _ -> navigate $ Route.Landing

    doLeaveOrDeleteRoomEv = doLeaveOrDeleteRoom <$> playersEv

  gameDetailsNut <- GameDetails.stateFulNut roomEnv

  D.div (bangCss "bg-gray-800 w-64 flex px-3 flex-col")
    [ D.div (bangCss "flex flex-col text-center ")
      [ D.span (bangCss "text-xl mt-6 text-blue-300") [ text $ (_.title) <$> roomEv]
      , D.span (bangCss "text-xs text-gray-300 mb-2") [ text_ $ "# " <> roomId]
      ]
    , if isAdmin
        then Btn.red "Delete room" (css "w-full mt-2") (doLeaveOrDeleteRoomEv)
        else Btn.red "Leave room" (css "w-full mt-2") (doLeaveOrDeleteRoomEv)
    , if isAdmin then gameDetailsNut else text_ ""
    ]


  where
  myId = (_.uid) $ unwrap self
  isAdmin = myId == roomId