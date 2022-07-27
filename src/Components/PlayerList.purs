module Components.PlayerList where

import Prelude

import App.Capa.Navigate (class Navigate, navigate)
import App.Route as Route
import App.Store.MyStore as MS
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Dumb.Button as Dumb.Button
import Dumb.VerticalListClickable as Dumb.VerticalListClickable
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, getStore)
import Models.Models (Player(..))
import Platform.Firebase.Firestore (getDocs)
import Platform.Html.CssUtils (css)

type State = { players :: Maybe (Array Player) }
data Action = Initialize | CreatePlayerClick

component
  :: ∀ q m. Navigate m => MonadAff m => MonadStore MS.Action MS.Store m => H.Component q Unit Void m
component =
  Hooks.component \_ _ -> Hooks.do
    players /\ playersId <- Hooks.useState Nothing

    Hooks.useLifecycleEffect do
      { fb } <- getStore
      players' <- H.liftAff $ getDocs fb.db "players"
      case players' of
        Left e -> do
          H.liftEffect $ log $ "Error in getDocs: " <> show e
          pure unit
        Right p -> Hooks.modify_ playersId $ const $ Just p
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