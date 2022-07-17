module Main where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Firebase.Auth (FirebaseAuth, firebaseAuthAff)
import Firebase.Config (FirebaseApp, firebaseAppAff)
import Firebase.Firestore (Firestore, firestoreDbAff, getPlayersAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Joyride.Firebase.Analytics (FirebaseAnalytics, firebaseAnalyticsAff)
import Models.Player (Player(..))

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action = Initialize | LoadPlayers Firestore

type FBEnv =
  { app :: FirebaseApp, analytics :: FirebaseAnalytics, db :: Firestore, auth :: FirebaseAuth }

css :: forall r i. String -> HH.IProp (class :: String | r) i
css = HP.class_ <<< HH.ClassName

type State = { fb :: Maybe FBEnv, players :: Array Player }

component :: forall q o m. MonadAff m => H.Component q Unit o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }
  where
  initialState _ = { fb: Nothing, players: [] }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { fb, players } =
    case fb of
      Nothing -> HH.div [ css "text-red-300 border-cyan-300" ] [ HH.text "Loading firebase" ]
      Just { db } ->
        HH.div
          [ css "border-gray-400 border-amber-300 border-2" ]
          [ HH.button [ HE.onClick \_ -> LoadPlayers db ] [ HH.text "Load Players" ]
          ,  HH.ul_ (renderPlayer <$> players)
          ]
    where
    renderPlayer (Player p) = HH.li_ [ HH.text p.name ]

  handleAction = case _ of
    Initialize -> do
      void $ H.fork do
        fb <- H.liftAff do
          app <- firebaseAppAff
          analytics <- firebaseAnalyticsAff app
          db <- firestoreDbAff app
          auth <- firebaseAuthAff app
          pure { app, analytics, db, auth }
        H.modify_ _ { fb = Just fb }
    LoadPlayers db -> do
      log "load players"
      players <- H.liftAff $ getPlayersAff db
      H.modify_ _ { players = players }
