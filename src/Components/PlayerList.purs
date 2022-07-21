module Components.PlayerList where

import Prelude

import Core.Capa.Navigate (class Navigate, navigate)
import Core.Route as Route
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Dumb.Button as Dumb
import Effect.Aff.Class (class MonadAff)
import Firebase.Firestore (getPlayersAff)
import HTML.Utils (css)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, getStore)
import Models.Player (Player(..))
import Store.MyStore as MS

type State = { players :: Maybe (Array Player) }
data Action = Initialize | CreatePlayerClick

component
  :: ∀ q m. Navigate m => MonadAff m => MonadStore MS.Action MS.Store m => H.Component q Unit Void m
component =
  Hooks.component \_ _ -> Hooks.do
    players /\ playersId <- Hooks.useState Nothing

    Hooks.useLifecycleEffect do
      { fb } <- getStore
      players' <- H.liftAff $ getPlayersAff fb.db
      Hooks.modify_ playersId $ const $ Just players'
      pure Nothing

    let handleCreatePlayerClick = navigate Route.CreatePlayer

    Hooks.pure do
      case players of
        Nothing -> HH.text "Loading players..."
        Just players' ->
          HH.div
            [ css "container mx-auto px-4" ]
            [ Dumb.button "Create Player" handleCreatePlayerClick
            , HH.text "Players:"
            , HH.ul_ (renderPlayer <$> players')
            ]
        where
        renderPlayer (Player p) = HH.li_ [ HH.text p.name ]
