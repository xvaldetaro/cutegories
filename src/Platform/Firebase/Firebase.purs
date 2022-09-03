module Platform.Firebase.Firebase where

import Prelude

import Effect.Aff (try)
import Models.Models (Chat)
import Platform.Firebase.Analytics (FirebaseAnalytics, firebaseAnalyticsAff)
import Platform.Firebase.Auth (FirebaseAuth, firebaseAuthAff)
import Platform.Firebase.Config (FirebaseApp, firebaseAppAff)
import Platform.Firebase.FbErr (mapFbErr)
import Platform.Firebase.Firestore.Common (Firestore, firestoreDbAff)
import Platform.Firebase.Synonyms (FbAff)

type FirebaseEnvR :: forall k. k -> Row Type -> Type
type FirebaseEnvR a r =
  { app :: FirebaseApp
  , analytics :: FirebaseAnalytics
  , db :: Firestore
  , auth :: FirebaseAuth
  | r
  }

type FirebaseEnv = FirebaseEnvR Chat ()

startFirebase :: FbAff FirebaseEnv
startFirebase = mapFbErr "startFirebase" <$> try do
  app <- firebaseAppAff
  analytics <- firebaseAnalyticsAff app
  db <- firestoreDbAff app
  auth <- firebaseAuthAff app
  pure { app, analytics, db, auth}