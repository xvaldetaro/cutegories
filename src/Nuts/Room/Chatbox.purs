module Nuts.Room.Chatbox where

import Prelude

import Control.Alt ((<|>))
import Core.Room.RoomManager (sendMessage)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (Either(..), hush)
import Data.Formatter.DateTime (format, parseFormatString)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((:=))
import Deku.Control (dyn, text, text_)
import Deku.Core (Nut, bussed, insert_)
import Deku.DOM as D
import Deku.Do as Doku
import Deku.Listeners (textInput)
import Models.Models (ChatMessage)
import Nuts.Dumb.Btn as Btn
import Nuts.Dumb.Input (inputCss, inputText)
import Nuts.Room.RoomEnv (RoomEnv)
import Paraglider.Operator.Combine (combineLatest)
import Platform.Deku.Html (bangCss, bangCss', css, enterUp)
import Platform.Deku.Misc (dynDiffOnlyAddition, useMemoBeh')

nut :: RoomEnv ->  Nut
nut {env, chatEv, playersEv, roomId} = Doku.do
  let
    chatCss = bangCss'
      [ css "grow overflow-y-auto scrollbar-thin scrollbar-thumb-rounded-full"
      , css "scrollbar-track-rounded-full scrollbar-thumb-gray-900 scrollbar-track-gray-800"
      , css "first:pt-6 shadow-md px-3"
      ]
  playerNamesEv <- useMemoBeh'
    (Map.fromFoldable <<< map (\{name, userId} -> Tuple userId name) <$> playersEv)

  D.div (bangCss "flex flex-col h-full grow")
    [ dynDiffOnlyAddition D.div chatCss (mkMessageRow playerNamesEv) chatEv
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

  dateText :: Number -> String
  dateText ts = fromMaybe "" do
    formatter <- hush $ parseFormatString "MM/DD/YY - hh:m a"
    dateTime <- toDateTime <$> (instant $ Milliseconds ts)
    pure $ format formatter dateTime

  mkMessageRow playerNamesEv { ts, sender, text: msgText } =
    let playerNameEv = playerNamesEv <#> \nameDict -> fromMaybe "" $ Map.lookup sender nameDict in
    D.div (bangCss "p-2 w-full justify-between ")
      [ D.div (bangCss "flex items-baseline")
          [ D.div (bangCss "font-medium text-gray-400 mr-2") [ text playerNameEv ]
          , D.div (bangCss "text-xs font-medium text-gray-400") [ text_ $ dateText ts ]
          ]
      , D.div (bangCss "text-gray-100") [ text_ msgText ]
      ]
