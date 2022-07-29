module Components.Room where

import Prelude

import App.Capa.Navigate (class Navigate)
import Components.Dumb.Styles (cardCss, cardCss')
import Control.Monad.Reader (class MonadAsk)
import Core.Room.RoomManager (observeRoom)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, hush)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Models.Models (Room(..))
import Platform.Firebase.Firebase (FirebaseEnv)
import Platform.Firebase.Firestore (FSError(..))
import Platform.Html.CssUtils (css)
import Platform.Rx.HooksExt as HooksExt

type Input = String

component
  :: ∀ q m r
   . MonadAsk { fb :: FirebaseEnv | r } m
  => Navigate m
  => MonadAff m
  => H.Component q Input Void m
component = Hooks.component \_ roomId -> Hooks.do
  room /\ roomStateId <- Hooks.useState $ Left FSLoading
  Hooks.useLifecycleEffect do
    roomUpdatedEvent <- observeRoom roomId
    void $ HooksExt.eventToState roomStateId $ roomUpdatedEvent
    pure Nothing

  Hooks.pure do
    either fsErrorUi resultUi room
    where
    resultUi (Room r) = HH.div
      [ cardCss "flex-col flex w-96 items-center px-8" ]
      [ HH.div [ css "text-lg font-bold mb-6 mt-6 text-slate-500" ] [ HH.text $ "Room: " <> r.title ]
      , HH.text $ show r
      ]

fsErrorUi :: ∀ props act. FSError -> HH.HTML props act
fsErrorUi = case _ of
  FSLoading -> HH.div [ cardCss' ] [ HH.text "Loading..."]
  e -> HH.div [ cardCss "text-red-700" ] [ HH.text $ "Error observing Room data:" <> show e ]

