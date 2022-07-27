module Platform.Firebase.Firebase where

import Prelude

import Effect.Aff (Aff)
import Platform.Firebase.Auth (FirebaseAuth, firebaseAuthAff)
import Platform.Firebase.Config (FirebaseApp, firebaseAppAff)
import Platform.Firebase.Firestore (Firestore, firestoreDbAff)
import Platform.Firebase.Analytics (FirebaseAnalytics, firebaseAnalyticsAff)

type FirebaseEnvR r =
  { app :: FirebaseApp, analytics :: FirebaseAnalytics, db :: Firestore, auth :: FirebaseAuth | r}

type FirebaseEnv = FirebaseEnvR ()


startFirebase :: Aff FirebaseEnv
startFirebase = do
  app <- firebaseAppAff
  analytics <- firebaseAnalyticsAff app
  db <- firestoreDbAff app
  auth <- firebaseAuthAff app
  pure { app, analytics, db, auth }