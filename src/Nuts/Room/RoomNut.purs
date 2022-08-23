module Nuts.Room.RoomNut where

import Prelude

import App.Env (Env, Nut_)
import Core.Room.RoomManager (observeRoom)
import Data.String (joinWith)
import Deku.Control (envy, text)
import Deku.DOM as D
import FRP.Event (AnEvent)
import Models.Models (Room(..))
import Nuts.Room.Chatbox as Chatbox
import Nuts.Room.RoomLeftBar as RoomLeftBar
import Paraglider.Operator.Take (take)
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (shareWild, wildSwitcher)
import Platform.Deku.QualifiedDo as QualifiedDo
import Platform.FRP.Wild (unliftDone)

nut :: ∀ s m l p. Env m -> String -> Nut_ s m l p
nut env@{fb} roomId = QualifiedDo.do
  wildRoom <- shareWild $ observeRoom fb roomId
  wildRoom # wildSwitcher (bangCss "h-full") \_ -> happy (unliftDone wildRoom)

  where
  happy :: AnEvent m Room -> _
  happy roomEv =
    let mkChatBox = \(Room {chatId}) -> Chatbox.nut env chatId in
    D.div (bangCss "flex h-full items-stretch [&>*]:pt-8")
      [ RoomLeftBar.nut env roomEv
      , envy D.div (bangCss "grow px-6 h-full")
        $ mkChatBox <$> (take 1 roomEv)
      , D.div (bangCss "w-64 px-3 flex flex-col border-l border-slate-300") [ rightBar ]
      ]
    where
    rightBar = text $ (\(Room room) -> joinWith " . - . " room.players) <$> roomEv
