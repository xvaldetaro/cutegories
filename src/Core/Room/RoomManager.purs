module Core.Room.RoomManager where

import Prelude

import App.Env (Env, FbEvent, mapFbEvent)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (lift, runExceptT)
import Data.Array (head)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (Either, note)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.String (toLower)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import FRP.Event (ZoraEvent)
import Models.Models (Chat, ChatMessage, ChatMessageIn, GameState(..), Player, PlayerId, PlayerIn, PlayerWithRef, Room, RoomId, RoomIn, UserId)
import Platform.FRP.FirebaseFRP (collectionEvent, docEvent)
import Platform.Firebase.FbErr (FbErr(..))
import Platform.Firebase.Firebase (FirebaseEnv)
import Platform.Firebase.Firestore.DocRef (DocRef)
import Platform.Firebase.Firestore.DocRef as DocRef
import Platform.Firebase.Firestore.QL as QL
import Platform.Firebase.Firestore.Query (Direction(..))
import Platform.Firebase.Firestore.Read (getDoc, queryDocs)
import Platform.Firebase.Firestore.Write (addDoc, deleteDoc, setDoc)
import Platform.Util.ErrorHandling (liftSuccess)
import Record as Record
import Type.Proxy (Proxy(..))

playersPath :: RoomId -> String
playersPath roomId = "rooms/" <> roomId <> "/players"

roomPath :: String
roomPath = "rooms"

chatPath :: RoomId -> String
chatPath roomId = "rooms/" <> roomId <> "/messages"

observeRoom :: FirebaseEnv -> RoomId -> FbEvent Room
observeRoom fb id = docEvent fb.db roomPath id

getRoom :: FirebaseEnv -> RoomId -> Aff (Either FbErr (Maybe Room))
getRoom fb id = getDoc fb.db roomPath id

queryPlayers :: FirebaseEnv -> RoomId -> Aff (Either FbErr (Array Player))
queryPlayers fb roomId = queryDocs fb.db q
  where
  q = QL.collection (playersPath roomId) []

queryMessages :: FirebaseEnv -> RoomId -> Aff (Either FbErr (Array ChatMessage))
queryMessages fb roomId = queryDocs fb.db q
  where
  q = QL.collection (chatPath roomId) []

getPlayerForUser :: FirebaseEnv -> UserId -> Aff (Either FbErr (Maybe PlayerWithRef))
getPlayerForUser fb userId = runExceptT do
  playerArr :: Array PlayerWithRef <- liftSuccess $ queryDocs fb.db q
  pure $ head playerArr
  where
  q = QL.group "players" [QL.whereFieldEquals "_id" userId]

observeRoomPlayers :: FirebaseEnv -> RoomId -> FbEvent (Array Player)
observeRoomPlayers fb roomId = sortPlayers $ collectionEvent fb.db q
  where
  -- Firestore orderBy is case sensitive so we need to sort manually here
  sortPlayers = mapFbEvent (Array.sortWith (\{name} -> toLower name))
  q = QL.collection (playersPath roomId) []

createRoom :: Env -> String -> String -> Aff (Either FbErr Unit)
createRoom { fb, self } myName title =
  let
    self' = unwrap self
    myId = self'.uid
    (room :: RoomIn ()) = { title, gameState: NeverStarted }
  in
  runExceptT do
    liftSuccess $ setDoc fb.db roomPath myId room
    liftSuccess $ addPlayerToRoom fb myId myId {name: myName}

addPlayerToRoom :: FirebaseEnv -> RoomId -> UserId -> PlayerIn () -> Aff (Either FbErr Unit)
addPlayerToRoom fb roomId userId playerIn = setDoc fb.db (playersPath roomId) userId playerPlusId
  where
  -- Need to add extra id to do group queries (https://stackoverflow.com/questions/56149601/firestore-collection-group-query-on-documentid)
  playerPlusId = Record.insert (Proxy :: _ "_id") userId playerIn

leaveOrDeleteRoom :: FirebaseEnv -> RoomId -> UserId -> Aff (Either FbErr Unit)
leaveOrDeleteRoom fb roomId userId =
  if roomId == userId
    then deleteRoom fb roomId
    else rmPlayerFromRoom fb roomId userId

deleteRoom :: FirebaseEnv -> RoomId -> Aff (Either FbErr Unit)
deleteRoom fb roomId = runExceptT do
  liftSuccess $ deleteRoomPlayers fb roomId
  liftSuccess $ deleteMessages fb roomId
  liftSuccess $ deleteDoc fb.db roomPath roomId

deleteRoomPlayers :: FirebaseEnv -> RoomId -> Aff (Either FbErr Unit)
deleteRoomPlayers fb roomId = runExceptT do
  players :: Array Player <- liftSuccess $ queryPlayers fb roomId
  deletionResults :: Array (Either FbErr Unit) <- lift
    $ traverse (\{id} -> rmPlayerFromRoom fb roomId id) players
  void $ liftEither $ sequence deletionResults

deleteMessages :: FirebaseEnv -> RoomId -> Aff (Either FbErr Unit)
deleteMessages fb roomId = runExceptT do
  messages :: Array ChatMessage <- liftSuccess $ queryMessages fb roomId
  deletionResults :: Array (Either FbErr Unit) <- lift
    $ traverse (\{id} -> deleteDoc fb.db (chatPath roomId) id) messages
  void $ liftEither $ sequence deletionResults

rmPlayerFromRoom :: FirebaseEnv -> RoomId -> UserId -> Aff (Either FbErr Unit)
rmPlayerFromRoom fb roomId userId = deleteDoc fb.db (playersPath roomId) userId

observeChat :: FirebaseEnv -> RoomId -> ZoraEvent (Either FbErr Chat)
observeChat fb roomId = collectionEvent fb.db q
  where
  q = QL.collection (chatPath roomId) [QL.orderByField "ts" Asc]

sendMessage
  :: Env
  -> RoomId
  -> String
  -> Aff (Either FbErr DocRef)
sendMessage { fb, self } roomId text = do
  chatMessage <- liftEffect mkMessage
  addDoc fb.db (chatPath roomId) chatMessage
  where
  mkMessage :: Effect (ChatMessageIn ())
  mkMessage = do
    ts <- unwrap <<< unInstant <$> now
    pure { ts, sender: (unwrap self).uid, text }
