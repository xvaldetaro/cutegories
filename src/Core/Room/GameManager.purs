module Core.Room.GameManager where

import Prelude

import App.Env (Env)
import Control.Monad.Except (runExceptT)
import Core.Room.RoomManager (gamePath)
import Data.Array (filter)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, head)
import Data.DateTime.Instant (unInstant)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.String (toLower)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Models.Models (Game, GameState(..), Guesses, Player, RoomId, Guess, blankGuesses)
import Platform.FRP.FirebaseFRP (docEvent)
import Platform.Firebase.Auth (uid)
import Platform.Firebase.Firebase (FirebaseEnv)
import Platform.Firebase.Firestore.Read (getDoc)
import Platform.Firebase.Firestore.Write (ArrayOp(..), ArrayUpdate, setDoc, updateDoc)
import Platform.Firebase.Synonyms (FbEvent, FbAff)
import Platform.Util.ErrorHandling (liftSuccess)

guessesPath :: String
guessesPath = "guesses"

observeGame :: FirebaseEnv -> RoomId -> FbEvent (Maybe Game)
observeGame fb id = docEvent fb.db gamePath id

startGame :: FirebaseEnv -> RoomId -> String -> Number -> FbAff Unit
startGame fb id topic durationSeconds = do
  start <- liftEffect $ unwrap <<< unInstant <$> now
  let
    endsAt = start + (durationSeconds * 1000.0)
    (game :: Game) = {topic, endGuesses: [], gameState: Started, ready: [], id, endsAt}
  log $ show [endsAt, start, durationSeconds]
  runExceptT do
    liftSuccess $ setDoc fb.db guessesPath id blankGuesses
    liftSuccess $ updateDoc fb.db gamePath id game ([]:: Array (ArrayUpdate String))

changeGameState :: FirebaseEnv -> RoomId -> GameState -> FbAff Unit
changeGameState fb id gameState = runExceptT do
  liftSuccess $ updateDoc fb.db gamePath id {gameState} ([]:: Array (ArrayUpdate String))

getSelfGuesses :: Env -> RoomId -> FbAff (Array Guess)
getSelfGuesses {fb, self} roomId = runExceptT do
  mbGuesses <- liftSuccess $ getGuesses fb roomId
  let
    guessesArr = maybe [] (\{guesses} -> guesses) mbGuesses
    myId = uid self
  pure $ filter (\{userId} -> userId == myId) guessesArr

type PlayerGuesses = {userId :: String, player :: Maybe Player, guesses :: NonEmptyArray String}
type SimilarGuess = { guess :: Guess, similarity :: Boolean }
type GuessCompendium = { byPlayer :: Array PlayerGuesses, similarityTable :: Map String SimilarGuess }

getGuessesByUser :: FirebaseEnv -> RoomId -> Array Player -> FbAff (Array PlayerGuesses)
getGuessesByUser fb roomId players = runExceptT do
  mbGuesses <- liftSuccess $ getGuesses fb roomId
  let
    playerMap = Map.fromFoldable $ (\p@{id} -> Tuple id p) <$> players

    guessesArr = maybe [] (\{guesses} -> guesses) mbGuesses

    grouped = Array.groupAllBy (\g1 g2 -> compare g1.userId g2.userId) guessesArr

    mkPlayerGuess :: NonEmptyArray Guess -> PlayerGuesses
    mkPlayerGuess guesses = {userId, player: Map.lookup userId playerMap, guesses: guessTexts}
      where
      guessTexts = (_.text) <$> guesses
      userId = (head guesses).userId
  pure (mkPlayerGuess <$> grouped)


getGuesses :: FirebaseEnv -> RoomId -> FbAff (Maybe Guesses)
getGuesses fb id = getDoc fb.db guessesPath id

-- waitingForReady :: Game -> Array Players -> Boolean
addGuess :: Env -> RoomId -> String -> FbAff Unit
addGuess { fb, self } roomId text = updateDoc fb.db guessesPath roomId {} [arrayUpdate]
  where
  arrayUpdate = { field: "guesses", op: ArrayUnion, elements: [guess]}
  guess = { userId: uid self, text: toLower text }


