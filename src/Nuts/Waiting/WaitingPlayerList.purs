module Nuts.Waiting.WaitingPlayerList where

import Prelude

import Control.Alt ((<|>))
import Core.Room.RoomManager (rmPlayerFromRoom)
import Data.Newtype (unwrap)
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Listeners (click)
import Effect.Aff (launchAff_)
import Nuts.Dumb.Btn as Btn
import Nuts.Room.RoomEnv (RoomEnv)
import Platform.Deku.Html (bangCss, combineCss, css)
import Platform.Deku.Misc (dynDiffOrdered)

nut :: RoomEnv -> Nut
nut { env: {fb, self}, roomId, roomEv, playersEv} =
  dynDiffOrdered D.ul (bangCss "flex flex-col") (_.id) rowUi playersEv

  where
  myId = (_.uid) $ unwrap self

  kickPlayer id = launchAff_ $ void $ rmPlayerFromRoom fb roomId id
  rowUi {name, id: userId} =
    let hiddenCss {id: admin} = if (admin /= myId) || (userId == myId) then css "hidden" else "" in
    let nameCss = if userId == myId then (css "text-white") else (css "text-gray-300") in
    D.li (bangCss "px-3 py-2 font-medium flex items-center justify-between")
      [ D.div (bangCss nameCss) [text_ name]
      , D.button
        ( (click $ pure (kickPlayer userId))
            <|> (combineCss
                [ pure $ Btn.baseCss
                    <> css "hover:bg-red-600 bg-red-800 text-slate-100 rounded-md py-0 px-2"
                , hiddenCss <$> roomEv
                ]
              )
        ) [text_ "Kick"]
      ]
