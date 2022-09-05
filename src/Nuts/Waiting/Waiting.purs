module Nuts.Waiting.Waiting where

import Prelude

import Data.Newtype (unwrap)
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Nuts.Room.RoomEnv (RoomEnv)
import Nuts.Waiting.Controls as WaitingControls
import Nuts.Waiting.GameDetails as GameDetails
import Nuts.Waiting.RoomChat as RoomChat
import Nuts.Waiting.WaitingPlayerList as WaitingPlayerList
import Platform.Deku.Html (bangCss)

nut :: RoomEnv -> Nut
nut roomEnv@{env: {self}, roomId, gameEv} = Doku.do
  D.div (bangCss "flex flex-col md:flex-row w-full h-full items-stretch bg-gray-800")
    [ D.div (bangCss "flex-grow overflow-auto w-full")
      [ WaitingControls.nut roomEnv
      , WaitingPlayerList.nut roomEnv
      , GameDetails.nut roomEnv
      ]
    , D.div (bangCss "flex-grow-0 rounded-t-xl h-64 bg-gray-700") [RoomChat.nut roomEnv]
    ]

  where
  myId = (_.uid) $ unwrap self
  isAdmin = myId == roomId
