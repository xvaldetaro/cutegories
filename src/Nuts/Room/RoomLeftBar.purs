module Nuts.Room.RoomLeftBar where

import Prelude

import App.Env (AppEvent(..))
import App.Navigation (navigate)
import App.Route as Route
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (lift, runExceptT)
import Core.Room.RoomManager (getPlayerForUser, leaveOrDeleteRoom, rmPlayerFromRoom, sendMessage)
import Data.Array (find, length)
import Data.Either (Either(..), note)
import Data.Newtype (unwrap)
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Listeners (click)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FRP.Event (toEvent)
import Nuts.Dumb.Btn as Btn
import Nuts.Room.RoomEnv (RoomEnv)
import Paraglider.Operator.ToAff (toAff)
import Platform.Deku.Html (bangCss, combineCss, css)
import Platform.Firebase.FbErr (FbErr(..))
import Platform.Util.ErrorHandling (liftSuccess)

nut :: RoomEnv -> Nut
nut { env: {fb, self, appPush}, playersEv, roomId, roomEv } =
  D.div (bangCss "bg-gray-800 w-64 flex px-3 flex-col")
    [ D.div_ [ text_ $ "Room #: " <> roomId ]
    , adminControls
    , nonAdminControls
    ]
  where
  myId = (_.uid) $ unwrap self

  -- TODO create game in Firebase
  createGame players =
    if hasEnoughPlayers players then
      log "creating game effect" *> (navigate $ Route.Game "createdGameId")
    else pure unit

  hasEnoughPlayers players = length players > 1
  canStartText can = if can then "Click to Start the game" else "Need more players to start"

  nonAdminHiddenCss {id} = if (id /= myId) then css "hidden" else ""
  adminHiddenCss {id} = if (id == myId) then css "hidden" else ""

  doLeaveOrDeleteRoom players = launchAff_ do
    eiResult <- leaveOrDeleteRoom fb roomId myId
    liftEffect $ case eiResult of
      Left e -> appPush $ ShowAppError $ show e
      Right _ -> navigate $ Route.Landing

  doLeaveOrDeleteRoomEv = doLeaveOrDeleteRoom <$> playersEv

  adminControls = D.div (combineCss [pure "flex flex-col w-full", nonAdminHiddenCss <$> roomEv])
    [ D.div (bangCss "mt-2") [ text $ canStartText <<< hasEnoughPlayers <$> playersEv ]
    , D.button
        ( (click $ createGame <$> playersEv)
            <|> combineCss
              [ pure (Btn.baseCss <> Btn.tealCss <> css "w-full")
              , (if _ then "" else css "hover:bg-red-100 bg-red-100 cursor-not-allowed") <<<
                  hasEnoughPlayers
                  <$> playersEv
              ]
        )
        [ text_ "Start Game" ]
    , Btn.gray "Delete room" (css "w-full mt-2") (doLeaveOrDeleteRoomEv)
    ]

  nonAdminControls = D.div (combineCss [pure "flex flex-col w-full", adminHiddenCss <$> roomEv])
    [ Btn.gray "Leave room" "w-full" (doLeaveOrDeleteRoomEv)
    ]