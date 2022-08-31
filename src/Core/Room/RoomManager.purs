module Core.Room.RoomManager where

import Prelude

import App.Env (Env, FbEvent)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (lift, runExceptT)
import Data.Array (head)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either, note)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import FRP.Event (ZoraEvent)
import Models.Models (Chat, ChatMessage, ChatMessageIn, Player, PlayerId, PlayerIn, Room, RoomId, RoomIn, PlayerWithRef)
import Platform.FRP.FirebaseFRP (collectionEvent, docEvent)
import Platform.Firebase.FbErr (FbErr(..))
import Platform.Firebase.Firebase (FirebaseEnv)
import Platform.Firebase.Firestore.Common (DocumentReference)
import Platform.Firebase.Firestore.DocRef (DocRef)
import Platform.Firebase.Firestore.DocRef as DocRef
import Platform.Firebase.Firestore.QL as QL
import Platform.Firebase.Firestore.Query (Direction(..))
import Platform.Firebase.Firestore.Query as Query
import Platform.Firebase.Firestore.Read (getDoc, queryDocs)
import Platform.Firebase.Firestore.Write (addDoc, deleteDoc)
import Platform.Undfnd.Undfnd (undfnd)
import Platform.Util.ErrorHandling (liftSuccess)

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

getPlayerForUser :: FirebaseEnv -> String -> Aff (Either FbErr (Maybe PlayerWithRef))
getPlayerForUser fb userId = runExceptT do
  playerArr :: Array PlayerWithRef <- liftSuccess $ queryDocs fb.db q
  pure $ head playerArr
  -- let mbRoomRef = mbPlayer >>= \{ref} -> DocRef.parent ref
  -- pure $ DocRef.id <$> mbRoomRef
  where
  q = QL.group "players" [QL.whereFieldEquals "userId" userId] # QL.ancestor 1

observeRoomPlayers :: FirebaseEnv -> RoomId -> FbEvent (Array Player)
observeRoomPlayers fb roomId = collectionEvent fb.db q
  where
  q = QL.collection (playersPath roomId) [QL.orderByField "name" Query.Asc]

createRoom :: Env -> String -> String -> Aff (Either FbErr DocRef)
createRoom { fb, self } myName title =
  let
    self' = unwrap self
    (room :: RoomIn ()) = { title, admin: self'.uid }
  in
  runExceptT do
    roomRef <- liftSuccess $ addDoc fb.db roomPath room
    void $ liftSuccess $ addPlayerToRoom fb (DocRef.id roomRef) {userId: self'.uid, name: myName}
    pure roomRef

leaveOrDeleteRoom :: FirebaseEnv -> RoomId -> Player -> Aff (Either FbErr Unit)
leaveOrDeleteRoom fb roomId player = runExceptT do
  mbRoom <- liftSuccess $ getRoom fb roomId
  room <- liftEither $ note (DocNotFound $ "room " <> roomId) mbRoom
  liftSuccess $ if room.admin == player.userId
    then deleteRoom fb roomId
    else rmPlayerFromRoom fb roomId player.id

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

addPlayerToRoom :: FirebaseEnv -> RoomId -> PlayerIn () -> Aff (Either FbErr DocRef)
addPlayerToRoom fb roomId playerIn = addDoc fb.db (playersPath roomId) playerIn

rmPlayerFromRoom :: FirebaseEnv -> RoomId -> PlayerId -> Aff (Either FbErr Unit)
rmPlayerFromRoom fb roomId playerId = deleteDoc fb.db (playersPath roomId) playerId

observeChat :: FirebaseEnv -> RoomId -> ZoraEvent (Either FbErr Chat)
observeChat fb roomId = collectionEvent fb.db q
  where
  q = QL.collection (chatPath roomId) [QL.orderByField "ts" Asc]

sendMessage
  :: Env
  -> RoomId
  -> String
  -> (Either FbErr DocRef -> Effect Unit)
  -> Effect Unit
sendMessage { fb, self } roomId text onDone = do
  chatMessage <- mkMessage
  launchAff_ do
    result <- addDoc fb.db (chatPath roomId) chatMessage
    liftEffect $ onDone result
  where
  mkMessage :: Effect (ChatMessageIn ())
  mkMessage = do
    ts <- unwrap <<< unInstant <$> now
    pure { ts, sender: (unwrap self).uid, text }
