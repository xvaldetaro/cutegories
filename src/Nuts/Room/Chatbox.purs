module Nuts.Room.Chatbox where

import Prelude

import Control.Alt ((<|>))
import Core.Room.RoomManager (sendMessage)
import Data.Array (drop, length)
import Data.Either (Either(..))
import Data.Foldable (oneOfMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Deku.Attribute ((:=))
import Deku.Control (dyn, text_)
import Deku.Core (Nut, bussed, insert_)
import Deku.DOM as D
import Deku.Listeners (textInput)
import FRP.Event (filterMap, withLast)
import Models.Models (ChatMessage)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Input (inputCss, inputText)
import Nuts.Room.RoomEnv (RoomEnv)
import Paraglider.Operator.SwitchMap (switchMap)
import Platform.Deku.Html (bangCss, bangCss', css, enterUp)

nut :: RoomEnv ->  Nut
nut {env, chatEv, roomId} = Doku.do
  let
    goNewMessages { last, now } =
      let
        last' = fromMaybe [] last
      in
        if length last' < length now then Just (drop (length last') now) else Nothing

    newMessagesEv = filterMap goNewMessages (withLast $ chatEv)

    rowsEv = switchMap (oneOfMap mkMessageRow) newMessagesEv

    chatCss = bangCss'
      [ css "grow overflow-y-auto scrollbar-thin scrollbar-thumb-rounded-full"
      , css "scrollbar-track-rounded-full scrollbar-thumb-gray-900 scrollbar-track-gray-800"
      , css "first:pt-6 shadow-md px-3"
      ]

  D.div (bangCss "flex flex-col h-full grow")
    [ dyn D.div chatCss rowsEv
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
      D.div (bangCss "flex mt-4 mb-6 px-3 w-full")
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
