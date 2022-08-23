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
import Paraglider.Operator.Combine (combineLatest)
import Paraglider.Operator.SwitchMap (switchMap)
import Platform.Deku.Html (bangCss, enterUp)
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
    [ dyn D.div (bangCss "bg-slate-50 grow") rowsEv
    , typeBox
    ]

  where
  typeBox = bussed \pushInputVal inputValEv -> bussed \pushClear clearEv ->
    let
        pushTextGo chat text = sendMessage fb chat text *> pushClear unit
        pushMessageTextEv = combineLatest pushTextGo chatEv inputValEv
    in
    D.div (bangCss "flex mt-4 mb-10 w-full")
      [ D.input
          ( bangCss "grow border rounded-md border-slate-500 mr-2 px-2 py-1"
            <|> (textInput $ pure pushInputVal)
              <|> (enterUp $ pushMessageTextEv)
                <|> ((\_ -> D.Value := "") <$> clearEv)
          )
          []
      , D.button
        ( (click $ pushMessageTextEv)
            <|> bangCss "hover:bg-teal-300 bg-teal-200 shadow-sm w-20 rounded-md"
        ) [text_ "Send"]
      ]

  mkMessageRow (ChatMessage {timestamp, playerId, text}) =
    pure $ pure $ insert_ $ D.div (bangCss "p-2 flex flex-col w-full justify-between ")
          [ D.div (bangCss "flex items-center")
            [ D.div (bangCss "border font-semibold rounded-xl px-1 bg-slate-200 mr-2") [text_ playerId]
            , D.div (bangCss "text-sm font-semibold") [text_ $ show timestamp]
            ]
          , D.div (bangCss "pl-1") [text_ text]
          ]
