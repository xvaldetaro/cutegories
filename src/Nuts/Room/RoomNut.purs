module Nuts.Room.RoomNut where

import Prelude

import App.Env (Env, forceDocPresent)
import Control.Alt ((<|>))
import Core.Room.RoomManager (getRoom, observeChat, observeRoom, observeRoomPlayers)
import Data.Either (Either, note)
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import Deku.Control (dyn, text_)
import Deku.Core (Nut, insert_, remove)
import Deku.DOM as D
import Deku.Do as Doku
import Models.Models (RoomId, Room)
import Nuts.Room.Chatbox as Chatbox
import Nuts.Room.RoomLeftBar as RoomLeftBar
import Nuts.Room.RoomRightBar as RoomRightBar
import Paraglider.Operator.FromAff (fromAff)
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (logE, logE', useCleanFbEvent)
import Platform.FRP.Led (loadingEvent)
import Platform.Firebase.FbErr (FbErr(..))

nut :: Env -> RoomId -> Nut
nut env@{ fb } roomId = Doku.do
  roomObsEv <- useCleanFbEvent env $ forceDocPresent <$> observeRoom fb roomId
  playersObsEv <- useCleanFbEvent env $ observeRoomPlayers fb roomId
  chatObsEv <- useCleanFbEvent env $ observeChat fb roomId

  let
    roomEnv =
      { env
      , roomId
      , roomEv: roomObsEv
      , playersEv: playersObsEv
      , chatEv: chatObsEv
      }
    loadingEv = loadingEvent $ roomObsEv *> playersObsEv *> chatObsEv

    happy =
      D.div (bangCss "flex h-full items-stretch")
        [ RoomLeftBar.nut roomEnv
        , Chatbox.nut roomEnv
        , RoomRightBar.nut roomEnv
        ]

    dynEvent = loadingEv <#> \isLoading -> if isLoading
      then
        (pure $ insert_ loadingDiv)
          <|> (loadingEv # filterMap \isLoading' -> if isLoading' then Nothing else Just remove)
      else (pure $ insert_ happy)

  dyn D.div (bangCss "h-full") dynEvent

  where
  loadingDiv = D.div_ [text_ "Loading..."]


