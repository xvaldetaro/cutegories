module Platform.Rx.HooksExt where

import Prelude

import Data.Maybe (Maybe(..))
import FRP.Event (Event)
import Halogen.Hooks (class HookNewtype, type (<>), UseState)
import Halogen.Hooks as Hooks
import Halogen.Subscription (Emitter, subscribe)

-- foreign import data UseFrpEvent :: Type -> Hooks.HookType

-- type UseFrpEvent' a = UseState (Maybe a) <> Hooks.Pure

-- instance newtypeUsePrevious :: HookNewtype ( a) (UsePrevious' a)

-- usePrevious :: forall m a. Event a -> Hook m (UsePrevious a) (Maybe a)
-- usePrevious emitter = Hooks.wrap hook
--   where
--   hook :: Hook m (UsePrevious' a) (Maybe a)
--   hook = Hooks.do
--     state /\ stateId <- Hooks.useState Nothing

--     { listener, emitter } <- H.liftEffect HS.create
--     subscribe emitter
--     Hooks.sub