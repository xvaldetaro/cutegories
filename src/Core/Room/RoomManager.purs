module Core.Room.RoomManager where

import Prelude

import App.Env (Env)
import Data.Array (sortWith)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Models.Models (Chat, ChatMessageIn, Player, Room, RoomId, RoomIn, PlayerId)
import Platform.FRP.FirebaseFRP (collectionEvent, docEvent)
import Platform.FRP.Wild (WildEvent)
import Platform.Firebase.Firebase (FirebaseEnv)
import Platform.Firebase.Firestore (ArrayOp(..), DocumentReference, FSError, OrderDirection(..), QueryConstraint(..), addDoc, updateDoc)

playerPath :: String
playerPath = "players"

roomPath :: String
roomPath = "rooms"

chatPath :: RoomId -> String
chatPath roomId = "rooms/" <> roomId <> "/messages"

observeRoom :: FirebaseEnv -> RoomId -> WildEvent FSError Room
observeRoom fb id = docEvent fb.db roomPath id

observeRoomPlayers :: FirebaseEnv -> Array PlayerId -> WildEvent FSError (Array Player)
observeRoomPlayers fb players = sortWith (_.name) <$> collectionEvent fb.db playerPath
  [ Ids players ]

createRoom :: Env -> String -> (Either FSError DocumentReference -> Effect Unit) -> Effect Unit
createRoom { fb, myId } title onDone =
  let
    (room :: RoomIn ()) = { title, admin: myId, players: [] }
  in
    launchAff_ do
      result <- addDoc fb.db roomPath room
      liftEffect $ onDone result

addPlayerToRoom
  :: FirebaseEnv -> PlayerId -> RoomId -> (Either FSError Unit -> Effect Unit) -> Effect Unit
addPlayerToRoom fb myId roomId onDone =
  launchAff_ do
    result <- updateDoc fb.db roomPath roomId {}
      [ { field: "players", op: ArrayUnion, elements: [ myId ] } ]
    liftEffect $ onDone result

rmPlayerFromRoom
  :: FirebaseEnv -> PlayerId -> RoomId -> (Either FSError Unit -> Effect Unit) -> Effect Unit
rmPlayerFromRoom fb myId roomId onDone =
  launchAff_ do
    result <- updateDoc fb.db roomPath roomId {}
      [ { field: "players", op: ArrayRemove, elements: [ myId ] } ]
    liftEffect $ onDone result

observeChat :: FirebaseEnv -> RoomId -> WildEvent FSError Chat
observeChat fb roomId = collectionEvent fb.db (chatPath roomId)
  [ OrderBy { field: "ts", direction: Asc } ]

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
    ts <- unwrap <<< unInstant <$> now
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