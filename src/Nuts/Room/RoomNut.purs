module Nuts.Room.RoomNut where

import Prelude

import App.Env (Env)
import Core.Room.RoomManager (observeRoom)
import Deku.Control (envy)
import Deku.Core (Nut)
import Deku.DOM as D
import FRP.Event (ZoraEvent)
import Models.Models (Room)
import Nuts.Room.Chatbox as Chatbox
import Nuts.Room.RoomLeftBar as RoomLeftBar
import Nuts.Room.RoomRightBar as RoomRightBar
import Paraglider.Operator.Take (take)
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (shareWild, wildSwitcherHappy)
import Platform.Deku.QualifiedDo as QualifiedDo

nut :: Env -> String -> Nut
nut env@{fb} roomId = QualifiedDo.do
  wildRoom <- shareWild $ observeRoom fb roomId
  wildRoom # wildSwitcherHappy (bangCss "h-full") happy

  where
  happy :: ZoraEvent Room -> _
  happy roomEv =
    let mkChatBox = \{id} -> Chatbox.nut env id in
    D.div (bangCss "flex h-full items-stretch")
      [ RoomLeftBar.nut env roomEv
      , envy D.div (bangCss "grow h-full") $ mkChatBox <$> (take 1 roomEv)
      , RoomRightBar.nut env roomEv
      ]