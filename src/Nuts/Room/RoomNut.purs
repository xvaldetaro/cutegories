module Nuts.Room.RoomNut where

import Prelude

import App.Env (Env)
import Control.Alt ((<|>))
import Core.Room.RoomManager (getRoom, observeChat, observeRoomPlayers)
import Data.Either (Either, note)
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import Deku.Control (dyn, text_)
import Deku.Core (Nut, insert, insert_, remove)
import Deku.DOM as D
import Deku.Do as Doku
import FRP.Event (fromEvent)
import Models.Models (RoomId, Room)
import Nuts.Room.Chatbox as Chatbox
import Nuts.Room.RoomLeftBar as RoomLeftBar
import Nuts.Room.RoomRightBar as RoomRightBar
import Paraglider.Operator.FromAff (fromAff)
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (useMemoBeh')
import Platform.FRP.Led (Led(..), dataEvent, errorEvent, led', loadingEvent)
import Platform.Firebase.FbErr (FbErr(..))

nut :: Env -> RoomId -> Nut
nut env@{ fb } roomId = Doku.do
  wildMbRoom <- useMemoBeh' $ fromEvent <<< fromAff $ getRoom fb roomId
  wildPlayers <- useMemoBeh' $ observeRoomPlayers fb roomId
  wildChat <- useMemoBeh' $ observeChat fb roomId

  let
    liftNoRoomError :: Either FbErr (Maybe Room) -> Either FbErr Room
    liftNoRoomError eiMbRoom = eiMbRoom >>= note (DocNotFound "room")

    Led _ errorRoom dataRoom = led' $ liftNoRoomError <$> wildMbRoom
    Led _ errorPlayers dataPlayers = led' wildPlayers
    Led _ errorChat dataChat = led' wildChat
    roomEnv =
      { env
      , roomId
      , roomEv: dataRoom
      , playersEv: dataPlayers
      , chatEv: dataChat
      }
    loadingEv = loadingEvent $ dataRoom *> wildPlayers *> wildChat
    errorEv = errorRoom <|> errorPlayers <|> errorChat

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
      else
        (errorEv <#> \e -> insert 0 (errorDiv e))
          <|> (pure $ insert_ happy)

  dyn D.div (bangCss "h-full") dynEvent

  where
  loadingDiv = D.div_ [text_ "Loading..."]
  errorDiv e = D.div_ [text_ $ "Error: " <> show e]
