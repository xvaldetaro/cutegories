module Core.Room.RoomManager where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Either (Either)
import FRP.Event (Event)
import Models.Models (Room)
import Platform.Firebase.Firebase (FirebaseEnv)
import Platform.Firebase.Firestore (FSError)
import Platform.Rx.FirebaseExt (docEvent)

roomPath :: String
roomPath = "rooms"

observeRoom
  :: ∀ m r
   . MonadAsk { fb :: FirebaseEnv | r } m
  => String
  -> m (Event (Either FSError Room))
observeRoom id = do
  { fb } <- ask
  pure $ docEvent fb.db roomPath id