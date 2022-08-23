module Core.Room.RoomManager where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Int (floor)
import Data.Newtype (unwrap)
import Deku.Core (class Korok)
import Effect (Effect)
import Effect.Now (now)
import Effect.Ref as Ref
import FRP.Event (fromEvent)
import Models.Models (Chat(..), ChatMessage(..), Player(..), Room(..))
import Platform.FRP.FirebaseFRP (docEvent)
import Platform.FRP.Wild (WildEvent, liftWild', liftWildWithLoading')
import Platform.Firebase.Firebase (FirebaseEnv)
import Platform.Firebase.Firestore (FSError)

roomPath :: String
roomPath = "rooms"

chatPath :: String
chatPath = "chats"

-- observeRoom :: ∀ s m. Korok s m => FirebaseEnv -> String -> WildEvent m FSError Room
-- observeRoom fb id = docEvent fb.db roomPath id

-- observeChat :: ∀ s m. Korok s m => FirebaseEnv -> String -> WildEvent m FSError Chat
-- observeChat fb id = docEvent fb.db chatPath id

-- sendMessage :: ∀ s m. Korok s m => FirebaseEnv -> String -> Chat -> Effect Unit
-- sendMessage fb chatId message =

mockRoom :: Room
mockRoom = Room
  { id: "mockRoomId"
  , title: "My Mock Room"
  , admin: "mockPlayer1"
  , players: [ "mockPlayer1", "mockPlayer2" ]
  , chatId: "mockChatId"
  }

mockChat :: Chat
mockChat = Chat
  { id: "mockChatId"
  , messages:
    [ ChatMessage {timestamp: 100, playerId: "mockPlayer1", text: "Mock message from player 1"}
    , ChatMessage {timestamp: 300, playerId: "mockPlayer2", text: "Mock message from player 2"}
    ]
  }

sendMessage :: FirebaseEnv -> Chat -> String -> Effect Unit
sendMessage {myId, bus} (Chat chat) text = do
  timestamp :: Int <- floor <<< unwrap <<< unInstant <$> now
  let msg = ChatMessage { timestamp, playerId: myId, text}
  let updatedChat = Chat $ chat {messages = Array.snoc chat.messages msg}
  bus.push updatedChat

observeRoom :: ∀ s m. Korok s m => FirebaseEnv -> String -> WildEvent m FSError Room
observeRoom _ _ = pure mockRoom

observeChat :: ∀ s m. Korok s m => FirebaseEnv -> String -> WildEvent m FSError Chat
observeChat {bus} id = liftWildWithLoading' $ fromEvent bus.event

-- getPlayers :: ∀ s m. Korok s m => FirebaseEnv -> Array String -> WildEvent m FSError (Array Player)
-- getPlayers {bus} id