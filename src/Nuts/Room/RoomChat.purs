module Nuts.Room.RoomChat where

import Prelude

import Core.Room.RoomManager (sendMessage)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (hush)
import Data.Formatter.DateTime (format, parseFormatString)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do as Doku
import Effect.Aff (launchAff_)
import Nuts.Dumb.Chatbox as Chatbox
import Nuts.Room.RoomEnv (RoomEnv)
import Platform.Deku.Html (bangCss)
import Platform.Deku.Misc (useMemoBeh')

nut :: RoomEnv ->  Nut
nut {env, chatEv, playersEv, roomId} = Doku.do
  playerNamesEv <- useMemoBeh' (Map.fromFoldable <<< map (\{name, id} -> Tuple id name) <$> playersEv)

  let

    mkMessageRow { ts, sender, text: msgText } =
      let playerNameEv = playerNamesEv <#> \nameDict -> fromMaybe "" $ Map.lookup sender nameDict in
      D.div (bangCss "p-2 w-full justify-between first:mt-auto")
        [ D.div (bangCss "flex items-baseline")
            [ D.div (bangCss "font-medium text-gray-400 mr-2") [ text playerNameEv ]
            , D.div (bangCss "text-xs font-medium text-gray-400") [ text_ $ dateText ts ]
            ]
        , D.div (bangCss "text-gray-100") [ text_ msgText ]
        ]

    doSendMessage text = launchAff_ $ void $ sendMessage env roomId text

  Chatbox.nut {renderRow: mkMessageRow, rowsEv: chatEv, sendMessageEv: pure doSendMessage}

  where
  dateText :: Number -> String
  dateText ts = fromMaybe "" do
    formatter <- hush $ parseFormatString "MM/DD/YY - hh:m a"
    dateTime <- toDateTime <$> (instant $ Milliseconds ts)
    pure $ format formatter dateTime

