module Nuts.Room.Chatbox where

import Prelude

import App.Env (Env, Nut_)
import Control.Alt ((<|>))
import Core.Room.RoomManager (observeChat, sendMessage)
import Data.Array (drop, length)
import Data.Foldable (oneOfMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Deku.Attribute ((:=))
import Deku.Control (dyn, text_)
import Deku.Core (class Korok, bussed, insert_)
import Deku.DOM as D
import Deku.Do as Doku
import Deku.Listeners (click, textInput)
import FRP.Event (AnEvent, filterMap, withLast)
import Models.Models (Chat, ChatMessage(..))
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Input (inputCss, inputText)
import Paraglider.Operator.Combine (combineLatest)
import Paraglider.Operator.SwitchMap (switchMap)
import Platform.Deku.Html (bangCss, css, enterUp)
import Platform.Deku.Misc (shareWild, wildSwitcher)
import Platform.FRP.Wild (WildEvent, unliftDone)
import Platform.Firebase.Firestore (FSError)

nut :: ∀ s m l p. Env m -> String -> Nut_ s m l p
nut env@{fb} chatId = Doku.do
  wildChat :: WildEvent m FSError Chat <- shareWild $ observeChat fb chatId
  wildChat # wildSwitcher (bangCss "h-full w-full")
    \_ -> happy env (unliftDone wildChat)

happy :: ∀ s m l p. Korok s m => Env m -> AnEvent m Chat -> Nut_ s m l p
happy {fb} chatEv = Doku.do
  let
      goNewMessages {last, now} =
        let last' = fromMaybe [] last in
        if length last' < length now then Just (drop (length last') now) else Nothing

      newMessagesEv = filterMap goNewMessages (withLast $ (_.messages) <<< unwrap <$> chatEv)

      rowsEv = switchMap (oneOfMap mkMessageRow) newMessagesEv

  D.div (bangCss "flex flex-col h-full")
    [ dyn D.div (bangCss "grow") rowsEv
    , typeBox
    ]

  where
  typeBox = bussed \pushInputVal inputValEv -> bussed \pushClear clearEv ->
    let
        pushTextGo chat text = sendMessage fb chat text *> pushClear unit
        pushMessageTextEv = combineLatest pushTextGo chatEv inputValEv
    in
    D.div (bangCss "flex mt-4 mb-10 w-full")
      [ inputText
          ( (bangCss $ inputCss <> "mr-2 grow")
            <|> (textInput $ pure pushInputVal)
              <|> (enterUp $ pushMessageTextEv)
                <|> ((\_ -> D.Value := "") <$> clearEv)
          )
      , Btn.gray "Send" (css "px-8") pushMessageTextEv
      ]

  mkMessageRow (ChatMessage {timestamp, playerId, text}) =
    pure $ pure $ insert_ $ D.div (bangCss "p-2 w-full justify-between ")
          [ D.div (bangCss "flex items-baseline")
            [ D.div (bangCss "font-medium text-gray-400 mr-2") [text_ playerId]
            , D.div (bangCss "text-xs font-medium text-gray-400") [text_ $ show timestamp]
            ]
          , D.div (bangCss "text-gray-100") [text_ text]
          ]
