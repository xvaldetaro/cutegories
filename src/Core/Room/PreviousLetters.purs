module Core.Room.PreviousLettersManager where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Maybe (Maybe, fromMaybe)
import Foreign.Object (Object)
import Foreign.Object as Object
import Models.Models (RoomId, PreviousLetters)
import Models.Paths (previousLettersPath)
import Platform.Firebase.Firebase (FirebaseEnv)
import Platform.Firebase.Firestore.Read (getDoc)
import Platform.Firebase.Firestore.Write (ArrayOp(..), setDoc, updateDoc)
import Platform.Firebase.Synonyms (FbAff)
import Platform.Util.ErrorHandling (liftSuccess)

getPreviousLetters :: FirebaseEnv -> RoomId -> String -> FbAff (Array String)
getPreviousLetters fb id topic = runExceptT do
  mb :: Maybe PreviousLetters <- liftSuccess $ getDoc fb.db previousLettersPath id
  let obj = fromMaybe Object.empty $ (_.byTopic) <$> mb
  pure $ fromMaybe [] $ Object.lookup topic obj

addNewLetter :: FirebaseEnv -> RoomId -> String -> String -> FbAff Unit
addNewLetter fb id topic letter =
  let
    au = [{ field: "byTopic." <> topic, op: ArrayUnion, elements: [letter] }]
  in
  updateDoc fb.db previousLettersPath id {} au

clearLetters :: FirebaseEnv -> RoomId -> String -> FbAff Unit
clearLetters fb id topic = runExceptT do
  mb :: Maybe PreviousLetters <- liftSuccess $ getDoc fb.db previousLettersPath id
  let obj = fromMaybe Object.empty $ (_.byTopic) <$> mb
  let upd = Object.insert topic [] obj
  liftSuccess $ setDoc fb.db previousLettersPath id upd