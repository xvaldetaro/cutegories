module Platform.Firebase.Firestore where

import Prelude

import Control.Alt ((<|>))
import Control.Promise (Promise, toAffE)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError, try)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn3, runEffectFn3)
import FRP.Event (Event, makeEvent)
import Foreign (Foreign)
import Models.Models (Chat(..), Player(..), PlayerInput(..), Room(..))
import Platform.Firebase.Auth (FirebaseAuth)
import Platform.Firebase.Config (FirebaseApp)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Simple.JSON as JSON

data Firestore

foreign import firestoreDb :: FirebaseApp -> Effect (Promise Firestore)

firestoreDbAff :: FirebaseApp -> Aff Firestore
firestoreDbAff = toAffE <<< firestoreDb

type DocumentReference =
  { id :: String
  , path :: String
  }

data DocumentSnapshot

class FirePath a where
  path :: a -> String

instance firePathPlayerInput :: FirePath PlayerInput where
  path _ = "players"
instance firePathRoom :: FirePath Room where
  path _ = "rooms"
instance firePathChat :: FirePath Chat where
  path _ = "chats"

foreign import addPlayer :: Firestore -> Foreign -> Effect (Promise DocumentReference)
foreign import removeUndefineds :: Foreign -> Foreign

addPlayerAff :: Firestore -> Player -> Aff DocumentReference
addPlayerAff fs r = toAffE $ addPlayer fs (removeUndefineds (JSON.writeImpl r))

foreign import getPlayer :: Firestore -> String -> Effect (Promise Foreign)
foreign import getPlayers :: Firestore -> Effect (Promise Foreign)

getPlayerAff :: Firestore -> String -> Aff (Maybe Player)
getPlayerAff fs id = do
  ds <- toAffE $ getPlayer fs id
  case JSON.read ds of
    Right r -> pure r
    Left e -> throwError (error (show e))

getPlayersAff :: Firestore -> Aff (Array Player)
getPlayersAff fs = do
  ds <- toAffE $ getPlayers fs
  case JSON.read ds of
    Right r -> pure r
    Left e -> throwError (error (show e))

foreign import addDoc_ :: EffectFn3 Firestore String Foreign (Promise DocumentReference)
-- foreign import getDoc_ :: Firestore -> DocumentReference -> Effect (Promise Foreign)
-- foreign import getDocs_ :: Firestore -> String -> Effect (Promise Foreign)

data FSError = ApiError String | JsonError String
derive instance genericFSError :: Generic FSError _
instance showFSError :: Show FSError where
  show = genericShow

addDoc
  :: ∀ a
   . WriteForeign a
  => Firestore
  -> String
  -> a
  -> Aff (Either FSError DocumentReference)
addDoc fs path x = do
  ei <- try $ toAffE $ (runEffectFn3 addDoc_) fs path (removeUndefineds (JSON.writeImpl x))
  pure $ lmap (ApiError <<< show) ei

-- getDoc :: ∀ a. ReadForeign a => Firestore -> DocumentReference -> Aff (Either FSError a)
-- getDoc fs docRef = do
--   eiErrorDoc <- try $ toAffE $ getDoc_ fs docRef
--   let eiConvertedDoc = lmap (ApiError <<< show) eiErrorDoc
--   pure $ eiConvertedDoc >>= \doc -> lmap (JsonError <<< show) (JSON.read doc)

-- getCollection :: ∀ a. ReadForeign a => Firestore -> String -> Aff (Either FSError (Array a))
-- getCollection fs path = do
--   eiErrorDocs <- try $ toAffE $ getDocs_ fs path
--   let eiConvertedDocs = lmap (ApiError <<< show) eiErrorDocs
--   pure $ eiConvertedDocs >>= \docs -> lmap (JsonError <<< show) (JSON.read docs)

-- createPlayerIfNotExistsYet :: Firestore -> String -> Aff String
-- createPlayerIfNotExistsYet fs id = getPlayerAff fs id >>= case _ of
--   Nothing -> _.id <$> addPlayerAff fs defaultPlayer
--   Just _ -> pure id

-- getLowestAvailablePlayer :: PlayerV0' -> Maybe Player
-- getLowestAvailablePlayer { player1, player2, player3, player4 } = (player4 `go` Player4)
--   <|> (player3 `go` Player3)
--   <|> (player2 `go` Player2)
--   <|> (player1 `go` Player1)
--   where
--   go Nothing = Just
--   go (Just _) = const Nothing

-- foreign import claimPlayer :: FirebaseAuth -> Firestore -> String -> String -> Effect (Promise Unit)

-- claimPlayerAff :: FirebaseAuth -> Firestore -> String -> Player -> Aff Unit
-- claimPlayerAff auth firestore myChannel p = toAffE
--   $ claimPlayer auth firestore myChannel
--   $ case p of
--       Player1 -> "player1"
--       Player2 -> "player2"
--       Player3 -> "player3"
--       Player4 -> "player4"

-- foreign import listenToRemoteChannelChanges :: Firestore -> String -> (Foreign -> Effect Unit) -> Effect (Promise (Effect Unit))

-- eventChannelChanges :: Firestore -> String -> Event Player
-- eventChannelChanges fs s = makeEvent \k -> do
--   u <- Ref.new (pure unit)
--   launchAff_ do
--     unsub <- toAffE $ listenToRemoteChannelChanges fs s (\f -> case JSON.read f of
--       Left e -> throwError (error (show e))
--       Right r -> k r)
--     liftEffect $ Ref.write unsub u
--   pure $ join $ Ref.read u

-- getPlayerForChannel :: FirebaseAuth -> Firestore -> String -> Maybe Player -> Aff (Maybe Player)
-- getPlayerForChannel = go 0
--   where
--   go n auth fs myChannel myPlayerHint = do
--     when (n >= 4) $ throwError (error "No player found")
--     player <- getPlayerAff fs myChannel
--     case player of
--       Nothing -> throwError (error "PROGRAMMING ERROR: Player has not been created yet, verify control flow!")
--       Just (PlayerV0 dt) -> do
--         let lowestPlayerForChannel = (myPlayerHint <|> getLowestAvailablePlayer dt)
--         case lowestPlayerForChannel of
--           -- we failed to find a player, which means that the game is full
--           Nothing -> pure Nothing
--           Just p -> do
--             -- we try to claim a player
--             -- usually this will succeed, but it will fail if two
--             -- are reaching for the same player concurrently
--             maybeClaim <- try (claimPlayerAff auth fs myChannel p)
--             case maybeClaim of
--               Right _ -> pure (Just p)
--               -- was claimed, try again
--               Left _ -> go (n + 1) auth fs myChannel myPlayerHint

-- foreign import sendMyPointsAndPenaltiesToFirebaseImpl :: Firestore -> FirebaseAuth -> String -> String -> String -> Number -> Number -> Effect (Promise Unit)

-- sendMyPointsAndPenaltiesToFirebase :: Firestore -> FirebaseAuth -> String -> Player -> Points -> Penalty -> Aff Unit
-- sendMyPointsAndPenaltiesToFirebase fdb auth myChannel myPlayer (Points points) (Penalty penalties) = do
--   toAffE $ sendMyPointsAndPenaltiesToFirebaseImpl fdb auth myChannel
--     ( case myPlayer of
--         Player1 -> "player1Points"
--         Player2 -> "player2Points"
--         Player3 -> "player3Points"
--         Player4 -> "player4Points"
--     )
--     ( case myPlayer of
--         Player1 -> "player1Penalties"
--         Player2 -> "player2Penalties"
--         Player3 -> "player3Penalties"
--         Player4 -> "player4Penalties"
--     )
--     points
--     penalties