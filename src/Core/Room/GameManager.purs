module Core.Room.GameManager where

import Prelude

import App.Env (Env)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (runExceptT)
import Data.Array (filter, foldl, (:))
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (note)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (toLower)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Now (now)
import Models.Models (Game, GameState(..), Guess, GuessMetadata, Guesses, Player, RoomId, blankGame, blankGuesses, blankValuation)
import Models.Paths (gamePath, guessesPath, valuationPath)
import Platform.FRP.FirebaseFRP (docEvent)
import Platform.Firebase.Auth (uid)
import Platform.Firebase.FbErr (FbErr(..))
import Platform.Firebase.Firebase (FirebaseEnv)
import Platform.Firebase.Firestore.Read (getDoc)
import Platform.Firebase.Firestore.Write (ArrayOp(..), ArrayUpdate, setDoc, updateDoc, updateDoc')
import Platform.Firebase.Synonyms (FbEvent, FbAff)
import Platform.Util.ErrorHandling (liftSuccess)
import Platform.Util.Similarity (findBestMatch, randomLetter)

observeGame :: FirebaseEnv -> RoomId -> FbEvent (Maybe Game)
observeGame fb id = docEvent fb.db gamePath id

startGame :: FirebaseEnv -> RoomId -> String -> Number -> Boolean -> FbAff Unit
startGame fb id topic durationSeconds addRandomLetter = do
  start <- liftEffect $ unwrap <<< unInstant <$> now
  rl <- liftEffect if addRandomLetter then (Just <$> randomLetter) else pure Nothing
  let
    endsAt = start + (durationSeconds * 1000.0)
    game = (blankGame id) { gameState = Started, topic = topic, endsAt = endsAt, randomLetter = rl }
  runExceptT do
    liftSuccess $ setDoc fb.db guessesPath id blankGuesses
    liftSuccess $ updateDoc' fb.db gamePath id game

setAllowNonAdmins :: FirebaseEnv -> RoomId -> Boolean -> FbAff Unit
setAllowNonAdmins fb roomId v = updateDoc' fb.db gamePath roomId {allowNonAdminToStartGame: v}

changeGameToResults :: FirebaseEnv -> RoomId -> Array Player -> FbAff Unit
changeGameToResults fb roomId allPlayers = runExceptT do
  guessMetadataArray <- liftSuccess $ getGuessMetadataArr fb roomId allPlayers
  liftSuccess $ setDoc fb.db valuationPath roomId blankValuation
  liftSuccess $ updateDoc' fb.db gamePath roomId {gameState: Results, guessMetadataArray}

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

getGuessMetadataArr :: FirebaseEnv -> RoomId -> Array Player -> FbAff (Array GuessMetadata)
getGuessMetadataArr fb roomId allPlayers = runExceptT do
  mbGuesses <- liftSuccess $ getGuesses fb roomId
  {guesses} <- liftEither $ note (DocNotFound "No guesses to get metadata") mbGuesses
  let
    playerMap :: Map String Player
    playerMap = Map.fromFoldable $ (\p@{id} -> Tuple id p) <$> allPlayers

    updateMap :: Map String (Array Player) -> Guess -> Map String (Array Player)
    updateMap playersByGuessId {text, userId} = case Map.lookup userId playerMap of
      Nothing -> playersByGuessId
      Just player ->
        let
          current = fromMaybe [] (Map.lookup text playersByGuessId)
          patched = player : current
        in Map.insert text patched playersByGuessId

    aggregatedPlayersByGuess :: Map String (Array Player)
    aggregatedPlayersByGuess = foldl updateMap Map.empty guesses

    uniqueTexts = Array.fromFoldable $ Map.keys aggregatedPlayersByGuess

    calcSimilarities :: String -> Array Player -> GuessMetadata
    calcSimilarities text players = {text, players, similars: findBestMatch text uniqueTexts}

  pure $ Array.sortWith (_.text)
    $ Array.fromFoldable
      $ mapWithIndex calcSimilarities aggregatedPlayersByGuess

getGuesses :: FirebaseEnv -> RoomId -> FbAff (Maybe Guesses)
getGuesses fb id = getDoc fb.db guessesPath id

-- waitingForReady :: Game -> Array Players -> Boolean
addGuess :: Env -> RoomId -> String -> FbAff Unit
addGuess { fb, self } roomId text = updateDoc fb.db guessesPath roomId {} [arrayUpdate]
  where
  arrayUpdate = { field: "guesses", op: ArrayUnion, elements: [guess]}
  guess = { userId: uid self, text: toLower text }


