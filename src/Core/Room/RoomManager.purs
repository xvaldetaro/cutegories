module Core.Room.RoomManager where

import Prelude

import App.Env (Env, mapFbEvent)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (lift, runExceptT)
import Data.Array (head)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String (toLower)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Random (random)
import Foreign.Object as Object
import Models.Models (Chat, ChatMessage, ChatMessageIn, Game, GameState(..), Player, PlayerIn, PlayerWithRef, Room, RoomId, RoomIn, UserId, PreviousLetters, blankGame)
import Models.Paths (chatPath, formsPersistPath, gamePath, guessesPath, playersPath, previousLettersPath, roomPath, valuationPath)
import Platform.FRP.FirebaseFRP (collectionEvent, docEvent)
import Platform.Firebase.FbErr (FbErr)
import Platform.Firebase.Firebase (FirebaseEnv)
import Platform.Firebase.Firestore.DocRef (DocRef)
import Platform.Firebase.Firestore.QL as QL
import Platform.Firebase.Firestore.Query (Direction(..))
import Platform.Firebase.Firestore.Read (getDoc, queryDocs)
import Platform.Firebase.Firestore.Write (ArrayOp(..), ArrayUpdate, addDoc, deleteDoc, setDoc, updateDoc)
import Platform.Firebase.Synonyms (FbEvent, FbAff)
import Platform.Util.ErrorHandling (liftSuccess)
import Record as Record
import Type.Proxy (Proxy(..))

observeRoom :: FirebaseEnv -> RoomId -> FbEvent (Maybe Room)
observeRoom fb id = docEvent fb.db roomPath id

getRoom :: FirebaseEnv -> RoomId -> FbAff (Maybe Room)
getRoom fb id = getDoc fb.db roomPath id

queryPlayers :: FirebaseEnv -> RoomId -> FbAff (Array Player)
queryPlayers fb roomId = queryDocs fb.db q
  where
  q = QL.collection (playersPath roomId) []

queryMessages :: FirebaseEnv -> RoomId -> FbAff (Array ChatMessage)
queryMessages fb roomId = queryDocs fb.db q
  where
  q = QL.collection (chatPath roomId) []

getPlayerForUser :: FirebaseEnv -> UserId -> FbAff (Maybe PlayerWithRef)
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

addScores :: FirebaseEnv -> RoomId -> String -> Array String -> FbAff Unit
addScores fb roomId categoryWithLetter winnerIds = do
  rid <- liftEffect random
  let
    winnerWithCategories = winnerIds <#> \playerId -> {playerId, round: categoryWithLetter, rid}
    au = if Array.length winnerIds == 0 then [] else
      [{ field: "scores", op: ArrayUnion, elements: winnerWithCategories}]
  updateDoc fb.db roomPath roomId {} au

setGameEndSnapshot :: FirebaseEnv -> RoomId -> Game -> FbAff Unit
setGameEndSnapshot fb roomId game = updateDoc fb.db roomPath roomId patch arr
  where
  patch = {gameEndSnapshot: Just game}
  (arr :: Array (ArrayUpdate String)) = []

createRoom :: Env -> String -> String -> FbAff Unit
createRoom { fb, self } myName title =
  let
    self' = unwrap self
    myId = self'.uid
    (room :: RoomIn ()) = { title, scores: [] }
  in
  runExceptT do
    liftSuccess $ setDoc fb.db roomPath myId room
    liftSuccess $ addPlayerToRoom fb myId myId {name: myName}
    liftSuccess $ setDoc fb.db gamePath myId (blankGame myId)
    liftSuccess $ setDoc fb.db previousLettersPath myId ({byTopic: Object.empty} :: PreviousLetters)

addPlayerToRoom :: FirebaseEnv -> RoomId -> UserId -> PlayerIn () -> FbAff Unit
addPlayerToRoom fb roomId userId playerIn = setDoc fb.db (playersPath roomId) userId playerPlusId
  where
  -- Need to add extra id to do group queries (https://stackoverflow.com/questions/56149601/firestore-collection-group-query-on-documentid)
  playerPlusId = Record.insert (Proxy :: _ "_id") userId playerIn

leaveOrDeleteRoom :: FirebaseEnv -> RoomId -> UserId -> FbAff Unit
leaveOrDeleteRoom fb roomId userId =
  if roomId == userId
    then deleteRoom fb roomId
    else rmPlayerFromRoom fb roomId userId

deleteRoom :: FirebaseEnv -> RoomId -> FbAff Unit
deleteRoom fb roomId = runExceptT do
  liftSuccess $ deleteRoomPlayers fb roomId
  liftSuccess $ deleteMessages fb roomId
  liftSuccess $ deleteDoc fb.db roomPath roomId
  liftSuccess $ deleteDoc fb.db gamePath roomId
  liftSuccess $ deleteDoc fb.db valuationPath roomId
  liftSuccess $ deleteDoc fb.db guessesPath roomId
  liftSuccess $ deleteDoc fb.db formsPersistPath roomId
  liftSuccess $ deleteDoc fb.db previousLettersPath roomId

deleteRoomPlayers :: FirebaseEnv -> RoomId -> FbAff Unit
deleteRoomPlayers fb roomId = runExceptT do
  players :: Array Player <- liftSuccess $ queryPlayers fb roomId
  deletionResults :: Array (Either FbErr Unit) <- lift
    $ traverse (\{id} -> rmPlayerFromRoom fb roomId id) players
  void $ liftEither $ sequence deletionResults

deleteMessages :: FirebaseEnv -> RoomId -> FbAff Unit
deleteMessages fb roomId = runExceptT do
  messages :: Array ChatMessage <- liftSuccess $ queryMessages fb roomId
  deletionResults :: Array (Either FbErr Unit) <- lift
    $ traverse (\{id} -> deleteDoc fb.db (chatPath roomId) id) messages
  void $ liftEither $ sequence deletionResults

rmPlayerFromRoom :: FirebaseEnv -> RoomId -> UserId -> FbAff Unit
rmPlayerFromRoom fb roomId userId = deleteDoc fb.db (playersPath roomId) userId

observeChat :: FirebaseEnv -> RoomId -> FbEvent Chat
observeChat fb roomId = collectionEvent fb.db q
  where
  q = QL.collection (chatPath roomId) [QL.orderByField "ts" Asc]

sendMessage :: Env -> RoomId -> String -> FbAff DocRef
sendMessage { fb, self } roomId text = do
  chatMessage <- liftEffect mkMessage
  addDoc fb.db (chatPath roomId) chatMessage
  where
  mkMessage :: Effect (ChatMessageIn ())
  mkMessage = do
    ts <- unwrap <<< unInstant <$> now
    pure { ts, sender: (unwrap self).uid, text }
