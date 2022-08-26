module App.Env where

import Prelude

import Control.Monad.Reader (Reader)
import Deku.Core (Nut)
import FRP.Event (ZoraEvent)
import Hyrule.Zora (Zora)
import Platform.Firebase.Firebase (FirebaseEnv)

data AppEvent

type Env =
  { fb :: FirebaseEnv
  , myId :: String

  -- App Bus
  , appEvent :: ZoraEvent AppEvent
  , appPush :: AppEvent -> Zora Unit
  }

type AppNut = Reader Env Nut

