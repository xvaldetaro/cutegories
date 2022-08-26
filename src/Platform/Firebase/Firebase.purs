module Platform.Firebase.Firebase where

import Prelude

import Effect.Aff (Aff)
import Models.Models (Chat)
import Platform.Firebase.Analytics (FirebaseAnalytics, firebaseAnalyticsAff)
import Platform.Firebase.Auth (FirebaseAuth, firebaseAuthAff)
import Platform.Firebase.Config (FirebaseApp, firebaseAppAff)
import Platform.Firebase.Firestore (Firestore, firestoreDbAff)

type FirebaseEnvR :: forall k. k -> Row Type -> Type
type FirebaseEnvR a r =
  { app :: FirebaseApp
  , analytics :: FirebaseAnalytics
  , db :: Firestore
  , auth :: FirebaseAuth
  , myId :: String
  -- Mock stuff
  -- , bus :: {push :: a -> Effect Unit, event :: Event a }
  | r
  }

type FirebaseEnv = FirebaseEnvR Chat ()

startFirebase :: Aff FirebaseEnv
startFirebase = do
  app <- firebaseAppAff
  analytics <- firebaseAnalyticsAff app
  db <- firestoreDbAff app
  auth <- firebaseAuthAff app
  pure { app, analytics, db, auth, myId: "7Mgc8HyJowTUe0gxLS3" }
-- let myId = "mockPlayer1"
-- {push, event: event'} <- create
-- {event} <- burning chat event'
-- log "startFirebase"
-- pure { myId, bus: {push, event} }