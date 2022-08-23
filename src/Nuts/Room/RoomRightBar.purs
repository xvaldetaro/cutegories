module Nuts.Room.RoomRightBar where

import Prelude

import App.Env (Env, Nut_)
import Control.Plus (empty)
import Deku.Control (text, text_)
import Deku.Core (class Korok)
import Deku.DOM as D
import FRP.Event (AnEvent)
import Models.Models (Player(..), Room(..))
import Nuts.Room.RoomPlayerList as RoomPlayerList
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (wildSwitcher)
import Platform.FRP.Wild (WildEvent, liftWildWithLoading', unliftDone)
import Platform.Firebase.Firestore (FSError)

nut :: ∀ s m l p. Env m -> AnEvent m Room -> Nut_ s m l p
nut env roomEv = Doku.do
  let wildPlayers = observePlayers roomEv
  let playersEv = unliftDone wildPlayers
  D.div (bangCss "flex flex-col")
    [ D.div (bangCss "text-lg font-bold") [text_ "Players"]
    , wildSwitcher empty (\_ -> RoomPlayerList.nut env roomEv playersEv ) wildPlayers
    ]

observePlayers :: ∀ s m. Korok s m => AnEvent m Room -> WildEvent m FSError (Array Player)
observePlayers roomEv = liftWildWithLoading' $ go <$> roomEv
  where
  go (Room {players}) = (\id -> Player { name: "Name Name", id }) <$> players