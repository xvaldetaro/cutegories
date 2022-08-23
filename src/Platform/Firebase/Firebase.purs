module Platform.Firebase.Firebase where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FRP.Event (Event, burning, create, subscribe)
import Models.Models (Chat)
import Paraglider.Operator.Replay (replayRefCount)

type FirebaseEnvR a r =
  {
  --   app :: FirebaseApp
  -- , analytics :: FirebaseAnalytics
  -- , db :: Firestore
  -- , auth :: FirebaseAuth
  -- Mock stuff
   myId :: String
  , bus :: {push :: a -> Effect Unit, event :: Event a }
  | r
  }

type FirebaseEnv = FirebaseEnvR Chat ()

startFirebase :: Chat -> Aff FirebaseEnv
startFirebase chat = liftEffect $ do
  -- app <- firebaseAppAff
  -- analytics <- firebaseAnalyticsAff app
  -- db <- firestoreDbAff app
  -- auth <- firebaseAuthAff app
  -- pure { app, analytics, db, auth }
  let myId = "fakePlayer1"
  {push, event: event'} <- create
  {event} <- burning chat event'
  log "startFirebase"
  pure { myId, bus: {push, event} }