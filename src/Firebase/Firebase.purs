module Firebase.Firebase where

import Prelude

import Effect.Aff (Aff)
import Firebase.Auth (FirebaseAuth, firebaseAuthAff)
import Firebase.Config (FirebaseApp, firebaseAppAff)
import Firebase.Firestore (Firestore, firestoreDbAff)
import Joyride.Firebase.Analytics (FirebaseAnalytics, firebaseAnalyticsAff)

type FirebaseEnv =
  { app :: FirebaseApp, analytics :: FirebaseAnalytics, db :: Firestore, auth :: FirebaseAuth }


startFirebase :: Aff FirebaseEnv
startFirebase = do
  app <- firebaseAppAff
  analytics <- firebaseAnalyticsAff app
  db <- firestoreDbAff app
  auth <- firebaseAuthAff app
  pure { app, analytics, db, auth }