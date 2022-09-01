module Core.Room.GameManager where

import Prelude

import App.Env (Env, FbEvent, mapFbEvent)
import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Except (lift, runExceptT)
import Core.Room.RoomManager (setGameEndSnapshot)
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
import Models.Models (Chat, ChatMessage, ChatMessageIn, GameState(..), Player, PlayerId, PlayerIn, PlayerWithRef, Room, RoomId, RoomIn, UserId, Game)
import Platform.FRP.FirebaseFRP (collectionEvent, docEvent)
import Platform.Firebase.Auth (uid)
import Platform.Firebase.FbErr (FbErr(..))
import Platform.Firebase.Firebase (FirebaseEnv)
import Platform.Firebase.Firestore.DocRef (DocRef)
import Platform.Firebase.Firestore.DocRef as DocRef
import Platform.Firebase.Firestore.QL as QL
import Platform.Firebase.Firestore.Query (Direction(..))
import Platform.Firebase.Firestore.Read (getDoc, queryDocs)
import Platform.Firebase.Firestore.Write (ArrayOp(..), addDoc, deleteDoc, setDoc, updateDoc)
import Platform.Undfnd.Undfnd (undfnd)
import Platform.Util.ErrorHandling (liftSuccess)
import Record as Record
import Type.Proxy (Proxy(..))

gamePath :: String
gamePath = "games"

getGame :: FirebaseEnv -> RoomId -> Aff (Either FbErr (Maybe Game))
getGame fb id = getDoc fb.db gamePath id

startGame :: FirebaseEnv -> RoomId -> String -> Aff (Either FbErr Unit)
startGame fb id topic = setDoc fb.db gamePath id game
  where
  (game :: Game) = {topic, guesses: []}

addGuess :: Env -> RoomId -> String -> Aff (Either FbErr Unit)
addGuess { fb, self } roomId text = updateDoc fb.db gamePath roomId {} [arrayUpdate]
  where
  arrayUpdate = { field: "guesses", op: ArrayUnion, elements: [guess]}
  guess = { userId: uid self, text }

endGame :: Env -> RoomId -> Aff (Either FbErr Unit)
endGame {fb, self} roomId = runExceptT do
  when (roomId /= uid self) $ throwError $ Unauthorized "Must be admin to end game!"
  mbGame <- liftSuccess $ getGame fb roomId
  game <- liftEither $ note (DocNotFound "endGame missing game!") mbGame
  liftSuccess $ setGameEndSnapshot fb roomId game

