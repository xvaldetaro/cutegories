module Platform.Firebase.Firestore where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError, try)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, runEffectFn2, runEffectFn3, runEffectFn4)
import Foreign (Foreign)
import Models.Models (Chat, Player, PlayerInput, Room)
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

foreign import removeUndefineds :: Foreign -> Foreign

foreign import addDoc_ :: EffectFn3 Firestore String Foreign (Promise DocumentReference)
foreign import getDoc_ :: EffectFn3 Firestore String String (Promise Foreign)
foreign import getDocs_ :: EffectFn2 Firestore String (Promise Foreign)
foreign import observeDoc_
  :: EffectFn4 Firestore String String (Foreign -> Effect Unit) (Promise Foreign)

data FSError = ApiError String | JsonError String

derive instance genericFSError :: Generic FSError _
instance showFSError :: Show FSError where
  show = genericShow

addDocF :: Firestore -> String -> Foreign -> Aff (Either FSError DocumentReference)
addDocF fs path x = do
  ei <- try $ toAffE $ (runEffectFn3 addDoc_) fs path (removeUndefineds x)
  pure $ lmap (ApiError <<< show) ei

addDoc
  :: ∀ a
   . WriteForeign a
  => Firestore
  -> String
  -> a
  -> Aff (Either FSError DocumentReference)
addDoc fs path x = addDocF fs path $ JSON.writeImpl x

getDocF :: Firestore -> String -> String -> Aff (Either FSError Foreign)
getDocF fs path id = do
  ei <- try $ toAffE $ (runEffectFn3 getDoc_) fs path id
  pure $ lmap (ApiError <<< show) ei

getDoc :: ∀ a. ReadForeign a => Firestore -> String -> String -> Aff (Either FSError a)
getDoc fs path id = parseDocResult <$> getDocF fs path id

getDocsF :: Firestore -> String -> Aff (Either FSError Foreign)
getDocsF fs path = do
  eiErrorDocs <- try $ toAffE $ (runEffectFn2 getDocs_) fs path
  pure $ lmap (ApiError <<< show) eiErrorDocs

getDocs :: ∀ a. ReadForeign a => Firestore -> String -> Aff (Either FSError (Array a))
getDocs fs path = parseDocResult <$> getDocsF fs path

-- observeDocs
--   :: ∀ a
--    . ReadForeign a
--   => Firestore
--   -> String
--   -> String
--   -> (a -> Effect Unit)
--   -> Aff (Either FSError (Array a))
-- observeDocs fs path id cb = do
--   eiErrorDocs <- try $ toAffE $ (runEffectFn4 observeDoc_) fs path id cb
--   let eiConvertedDocs = lmap (ApiError <<< show) eiErrorDocs
--   pure $ eiConvertedDocs >>= \docs -> lmap (JsonError <<< show) (JSON.read docs)

parseDocResult :: ∀ a. ReadForeign a => Either FSError Foreign -> Either FSError  a
parseDocResult eiErrorDoc =  (lmap (ApiError <<< show) eiErrorDoc)
    >>= \doc -> lmap (JsonError <<< show) (JSON.read doc)

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