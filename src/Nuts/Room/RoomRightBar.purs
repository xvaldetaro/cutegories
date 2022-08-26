module Nuts.Room.RoomRightBar where

import Prelude

import App.Env (Env)
import Control.Plus (empty)
import Core.Room.RoomManager (observeRoomPlayers)
import Data.Maybe (Maybe(..))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do as Doku
import FRP.Event (ZoraEvent)
import Models.Models (Player, Room)
import Nuts.Room.RoomPlayerList as RoomPlayerList
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (shareWild, wildSwitcher)
import Platform.FRP.Wild (WildEvent, liftWild')
import Platform.Firebase.Firestore (FSError)

nut :: Env -> ZoraEvent Room -> Nut
nut env roomEv = Doku.do
  wildPlayers <- shareWild observePlayers
  D.div (bangCss "bg-gray-800 w-64 px-3 flex flex-col")
    [ D.div (bangCss "text-lg font-semibold") [ text_ "Players" ]
    , wildPlayers # wildSwitcher empty
        { happy: RoomPlayerList.nut env roomEv, loading: Nothing, error: Nothing }
    ]

  where
  observePlayers :: WildEvent FSError (Array Player)
  observePlayers = do
    {players} <- (liftWild' roomEv)
    observeRoomPlayers env.fb players