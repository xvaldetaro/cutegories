module Core.Room.FormsPersistManager where

import Prelude

import Data.Maybe (fromMaybe)
import Models.Models (FormsPersist, RoomId, blankFormsPersist)
import Models.Paths (formsPersistPath)
import Platform.Firebase.Firebase (FirebaseEnv)
import Platform.Firebase.Firestore.Read (getDoc)
import Platform.Firebase.Firestore.Write (setDoc)
import Platform.Firebase.Synonyms (FbAff)

getFormsPersist :: FirebaseEnv -> RoomId -> FbAff FormsPersist
getFormsPersist fb id = map (map (fromMaybe blankFormsPersist)) $ getDoc fb.db formsPersistPath id

saveFormsPersist :: FirebaseEnv -> RoomId -> FormsPersist -> FbAff Unit
saveFormsPersist fb id fope = setDoc fb.db formsPersistPath id fope
