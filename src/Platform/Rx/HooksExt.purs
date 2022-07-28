module Platform.Rx.HooksExt (eventToState, useFrpEvent, useFrpEvent', UseFrpEvent) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect)
import Effect.Ref as Ref
import FRP.Event (Event)
import Halogen as H
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookM, StateId, UseEffect, UseRef, UseState)
import Halogen.Hooks as Hooks
import Halogen.Query.HalogenM (SubscriptionId)
import HyruleRx as Rx

foreign import data UseFrpEvent :: Type -> Hooks.HookType

type UseFrpEvent' a = UseState a <> UseRef (Maybe SubscriptionId) <> UseEffect <> Hooks.Pure

instance newtypeUsePrevious :: HookNewtype (UseFrpEvent a) (UseFrpEvent' a)

useFrpEvent
  :: ∀ m a. MonadEffect m => a -> Event a -> Hook m (UseFrpEvent a) (a /\ Maybe SubscriptionId)
useFrpEvent initialState event = Hooks.wrap hook
  where
  hook :: Hook m (UseFrpEvent' a) (a /\ Maybe SubscriptionId)
  hook = Hooks.do
    state /\ stateId <- Hooks.useState initialState
    sub /\ subRef <- Hooks.useRef Nothing

    Hooks.useLifecycleEffect do
      sub' <- Hooks.subscribe $ Rx.toHalo $ Hooks.put stateId <$> event
      H.liftEffect $ Ref.write (Just sub') subRef
      pure Nothing

    Hooks.pure $ state /\ sub

useFrpEvent' :: ∀ m a. MonadEffect m => a -> Event a -> Hook m (UseFrpEvent a) a
useFrpEvent' a e = fst <$> useFrpEvent a e

eventToState :: ∀ m state. StateId state -> Event state -> HookM m H.SubscriptionId
eventToState sId event = Hooks.subscribe $ Rx.toHalo $ Hooks.put sId <$> event