module Components.Room where

import Prelude

import App.Capa.Navigate (class Navigate)
import Components.Dumb.Styles (cardCss)
import Control.Monad.Reader (class MonadAsk)
import Core.Room.RoomManager (observeRoom)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
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
    HH.div
      [ cardCss "flex-col flex w-96 items-center px-8" ]
      [ HH.div [ css "text-lg font-bold mb-6 mt-6 text-slate-500" ] [ HH.text $ "Room: " <> roomId ]
      , HH.text $ show room
      ]