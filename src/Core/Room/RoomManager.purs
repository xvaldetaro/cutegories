module Core.Room.RoomManager where

import Prelude

import App.Env (Env)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Int (floor)
import Data.Newtype (unwrap)

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref as Ref
import FRP.Event (ZoraEvent, fromEvent)
import Models.Models (Chat(..), ChatMessage(..), ChatMessageIn, Player(..), Room(..), RoomId)
import Platform.FRP.FirebaseFRP (addDocEvent, collectionEvent, docEvent)
import Platform.FRP.Wild (Wild, WildEvent, liftWild', liftWildWithLoading')
import Platform.Firebase.Firebase (FirebaseEnv)
import Platform.Firebase.Firestore (DocumentReference, FSError, addDoc)

roomPath :: String
roomPath = "rooms"

chatPath :: RoomId -> String
chatPath roomId = "rooms/" <> roomId <> "/messages"

observeRoom ::FirebaseEnv -> RoomId -> WildEvent FSError Room
observeRoom fb id = docEvent fb.db roomPath id

observeChat ::FirebaseEnv -> RoomId -> WildEvent FSError Chat
observeChat fb roomId = collectionEvent fb.db (chatPath roomId)

sendMessage
  :: Env
  -> RoomId
  -> String
  -> (Either FSError DocumentReference -> Effect Unit)
  -> Effect Unit
sendMessage { fb, myId } roomId text onDone = do
  chatMessage <- mkMessage
  launchAff_ do
    result <- addDoc fb.db (chatPath roomId) chatMessage
    liftEffect $ onDone result
  where
  mkMessage :: Effect (ChatMessageIn ())
  mkMessage = do
    ts <- floor <<< unwrap <<< unInstant <$> now
    pure { ts, sender: myId, text }

-- mockRoom :: Room
-- mockRoom =
--   { id: "mockRoomId"
--   , title: "My Mock Room"
--   , admin: "mockPlayer1"
--   , players: [ "mockPlayer1", "mockPlayer2" ]
--   , chatId: "mockChatId"
--   }

-- mockChat :: Chat
-- mockChat =
--   { id: "mockChatId"
--   , messages:
--     [ { timestamp: 100, playerId: "mockPlayer1", text: "Mock message from player 1"}
--     , { timestamp: 300, playerId: "mockPlayer2", text: "Mock message from player 2"}
--     ]
--   }

-- sendMessage :: FirebaseEnv -> Chat -> String -> Effect Unit
-- sendMessage {myId, bus} chat text = do
--   timestamp :: Int <- floor <<< unwrap <<< unInstant <$> now
--   let msg = { timestamp, playerId: myId, text}
--   let updatedChat = chat {messages = Array.snoc chat.messages msg}
--   bus.push updatedChat

-- observeRoom ::FirebaseEnv -> String -> WildEvent FSError Room
-- observeRoom _ _ = pure mockRoom

-- observeChat ::FirebaseEnv -> String -> WildEvent FSError Chat
-- observeChat {bus} id = liftWildWithLoading' $ fromEvent bus.event

-- getPlayers ::FirebaseEnv -> Array String -> WildEvent FSError (Array Player)
-- getPlayers {bus} id