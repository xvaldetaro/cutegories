module Components.PlayerList where

import Prelude

import Data.Const (Const(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Firebase.Firebase (FirebaseEnv, startFirebase)
import Firebase.Firestore (Firestore, getPlayersAff)
import HTML.Utils (css)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Monad (class MonadStore)
import Models.Player (Player(..))
import Store.MyStore as MS

type State = { fb :: Maybe FirebaseEnv, players :: Array Player }
data Action = Initialize | LoadPlayers Firestore

component :: ∀ q m. MonadAff m => MonadStore MS.Action MS.Store m => H.Component q Unit Void m
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
          [ css "border-gray-400 border-2" ]
          [ HH.button [ HE.onClick \_ -> LoadPlayers db ] [ HH.text "Load Players" ]
          , HH.ul_ (renderPlayer <$> players)
          ]
    where
    renderPlayer (Player p) = HH.li_ [ HH.text p.name ]

  handleAction = case _ of
    Initialize -> do
      void $ H.fork do
        fb <- H.liftAff startFirebase
        H.modify_ _ { fb = Just fb }
    LoadPlayers db -> do
      log "load players"
      players <- H.liftAff $ getPlayersAff db
      H.modify_ _ { players = players }