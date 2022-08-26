module Nuts.Room.RoomRightBar where

import Prelude

import App.Env (Env)
import Control.Plus (empty)
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import FRP.Event (ZoraEvent)
import Models.Models (Player, Room)
import Nuts.Room.RoomPlayerList as RoomPlayerList
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (wildSwitcher)
import Platform.FRP.Wild (WildEvent, liftWildWithLoading', unliftDone)
import Platform.Firebase.Firestore (FSError)

nut :: Env -> ZoraEvent Room -> Nut
nut env roomEv = Doku.do
  let wildPlayers = observePlayers roomEv
  let playersEv = unliftDone wildPlayers
  D.div (bangCss "bg-gray-800 w-64 px-3 flex flex-col")
    [ D.div (bangCss "text-lg font-semibold") [text_ "Players"]
    , wildSwitcher empty (\_ -> RoomPlayerList.nut env roomEv playersEv ) wildPlayers
    ]

observePlayers :: ZoraEvent Room -> WildEvent FSError (Array Player)
observePlayers roomEv = liftWildWithLoading' $ go <$> roomEv
  where
  go {players} = (\id -> { name: "Name Name", id }) <$> players