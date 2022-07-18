module Components.PlayerList where

import Prelude

import Core.Capa.Navigate (class Navigate, navigate)
import Core.Route as Route
import Data.Maybe (Maybe(..))
import Dumb.Button as Dumb
import Effect.Aff.Class (class MonadAff)
import Firebase.Firestore (getPlayersAff)
import HTML.Utils (css)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Monad (class MonadStore, getStore)
import Models.Player (Player(..))
import Store.MyStore as MS

type State = { players :: Maybe (Array Player) }
data Action = Initialize | CreatePlayerClick

component
  :: ∀ q m. Navigate m => MonadAff m => MonadStore MS.Action MS.Store m => H.Component q Unit Void m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }
  where
  initialState _ = { players: Nothing }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { players } = case players of
    Nothing -> HH.text "Loading players..."
    Just players' ->
      HH.div
        [ css "container mx-auto px-4" ]
        [ Dumb.button "Create Player" CreatePlayerClick
        , HH.text "Players:"
        , HH.ul_ (renderPlayer <$> players')
        ]
    where
    renderPlayer (Player p) = HH.li_ [ HH.text p.name ]

  handleAction = case _ of
    Initialize -> do
      { fb } <- getStore
      void $ H.fork $ do
        players <- H.liftAff $ getPlayersAff fb.db
        H.modify_ _ { players = Just players }
    CreatePlayerClick -> navigate Route.CreatePlayer
