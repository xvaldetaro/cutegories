module App.Env where

import Prelude

import Control.Monad.Reader (Reader)
import Deku.Core (class Korok, Domable)
import FRP.Event (AnEvent)
import FRP.Event.VBus (V)
import Platform.Firebase.Firebase (FirebaseEnv)

data AppEvent

type Env m =
  { fb :: FirebaseEnv

  -- App Bus
  , appEvent :: AnEvent m AppEvent
  , appPush :: AppEvent -> m Unit
  }

type AppNut = ∀ s m lock payload. Korok s m => Reader (Env m) (Domable m lock payload)

