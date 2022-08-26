module Nuts.Room.Chatbox where

import Prelude

import App.Env (Env)
import Control.Alt ((<|>))
import Core.Room.RoomManager (observeChat, sendMessage)
import Data.Array (drop, length)
import Data.Either (Either(..))
import Data.Foldable (oneOfMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Deku.Attribute ((:=))
import Deku.Control (dyn, text_)
import Deku.Core (Nut, bussed, insert_)
import Deku.DOM as D
import Deku.Do as Doku
import Deku.Listeners (textInput)
import FRP.Event (ZoraEvent, filterMap, withLast)
import Models.Models (Chat, ChatMessage, RoomId)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Input (inputCss, inputText)
import Paraglider.Operator.SwitchMap (switchMap)
import Platform.Deku.Html (bangCss, css, enterUp)
import Platform.Deku.Misc (shareWild, wildSwitcher)
import Platform.FRP.Wild (WildEvent, unliftDone)
import Platform.Firebase.Firestore (FSError)

nut :: Env -> RoomId -> Nut
nut env@{ fb } roomId = Doku.do
  wildChat :: WildEvent FSError Chat <- shareWild $ observeChat fb roomId
  wildChat # wildSwitcher (bangCss "h-full w-full")
    \_ -> happy env roomId (unliftDone wildChat)

happy :: Env -> RoomId -> ZoraEvent Chat -> Nut
happy env roomId chatEv = Doku.do
  let
    goNewMessages { last, now } =
      let
        last' = fromMaybe [] last
      in
        if length last' < length now then Just (drop (length last') now) else Nothing

    newMessagesEv = filterMap goNewMessages (withLast $ chatEv)

    rowsEv = switchMap (oneOfMap mkMessageRow) newMessagesEv

  D.div (bangCss "flex flex-col h-full")
    [ dyn D.div (bangCss "grow") rowsEv
    , typeBox
    ]

  where
  typeBox :: Nut
  typeBox = bussed \pushInputVal inputValEv -> bussed \pushClear clearEv -> bussed \pushErr errEv ->
    let
      onMessageSent (Left e) = pushErr $ show e
      onMessageSent _ = pure unit
      errElem = (\e -> pure $ insert_ $ text_ e) <$> errEv
      pushTextGo text = sendMessage env roomId text onMessageSent *> pushClear unit
      pushMessageTextEv = pushTextGo <$> inputValEv
    in
      D.div (bangCss "flex mt-4 mb-10 w-full")
        [ inputText
            ( (bangCss $ inputCss <> "mr-2 grow")
                <|> (textInput $ pure pushInputVal)
                <|> (enterUp $ pushMessageTextEv)
                <|> ((\_ -> D.Value := "") <$> clearEv)
            )
        , Btn.gray "Send" (css "px-8") pushMessageTextEv
        , dyn D.div (bangCss "text-red-500") errElem
        ]

  mkMessageRow ({ ts, sender, text } :: ChatMessage) =
    pure $ pure $ insert_ $ D.div (bangCss "p-2 w-full justify-between ")
      [ D.div (bangCss "flex items-baseline")
          [ D.div (bangCss "font-medium text-gray-400 mr-2") [ text_ sender ]
          , D.div (bangCss "text-xs font-medium text-gray-400") [ text_ $ show ts ]
          ]
      , D.div (bangCss "text-gray-100") [ text_ text ]
      ]
