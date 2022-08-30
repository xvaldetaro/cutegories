module Nuts.Room.RoomRightBar where


import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Nuts.Room.RoomEnv (RoomEnv)
import Nuts.Room.RoomPlayerList as RoomPlayerList
import Platform.Deku.Html (bangCss)

nut :: RoomEnv -> Nut
nut roomEnv= Doku.do
  D.div (bangCss "bg-gray-800 w-64 px-3 flex flex-col")
    [ D.div (bangCss "text-lg font-semibold") [ text_ "Players" ]
    , RoomPlayerList.nut roomEnv
    ]