module Core.Room.RoomManager where

import Prelude

import App.Env (Env, FbEvent)
import Control.Monad.Except (runExceptT)
import Data.Array (head)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import FRP.Event (ZoraEvent)
import Models.Models (Chat, ChatMessageIn, Player, PlayerId, PlayerIn, PlayerWithRef, Room, RoomIn, RoomId)
import Platform.FRP.FirebaseFRP (collectionEvent, docEvent)
import Platform.Firebase.FbErr (FbErr)
import Platform.Firebase.Firebase (FirebaseEnv)
import Platform.Firebase.Firestore.Common (DocumentReference)
import Platform.Firebase.Firestore.DocRef (DocRef)
import Platform.Firebase.Firestore.DocRef as DocRef
import Platform.Firebase.Firestore.QL as QL
import Platform.Firebase.Firestore.Query (Direction(..))
import Platform.Firebase.Firestore.Query as Query
import Platform.Firebase.Firestore.Read (getDoc, queryDocs)
import Platform.Firebase.Firestore.Write (addDoc, deleteDoc)
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

getRoomForUserId :: FirebaseEnv -> String -> Aff (Either FbErr (Maybe RoomId))
getRoomForUserId fb userId = runExceptT do
  playerArr :: Array {ref :: DocRef} <- liftSuccess $ queryDocs fb.db q
  let mbPlayer = head playerArr
  let mbRoomRef = mbPlayer >>= \{ref} -> DocRef.parent ref
  pure $ DocRef.id <$> mbRoomRef
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
