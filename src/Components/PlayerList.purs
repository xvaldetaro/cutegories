module Components.PlayerList where

import Prelude

import App.Capa.Navigate (class Navigate, navigate)
import App.Route as Route
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Dumb.Button as Dumb.Button
import Dumb.VerticalListClickable as Dumb.VerticalListClickable
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Platform.Firebase.Firestore (getPlayersAff)
import Platform.Html.CssUtils (css)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, getStore)
import Models.Models (Player(..))
import App.Store.MyStore as MS

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
    let handlePlayerClick p = log $ "player clicked: " <> show p

    Hooks.pure do
      case players of
        Nothing -> HH.text "Loading players..."
        Just players' ->
          HH.div
            [ css "container mx-auto px-4" ]
            [ Dumb.Button.button "Create Player" handleCreatePlayerClick
            , HH.text "Players:"
            , Dumb.VerticalListClickable.verticalListClickable playerItems handlePlayerClick
            ]
          where
          playerItems = (\(Player p) -> {text: p.name}) <$> players'