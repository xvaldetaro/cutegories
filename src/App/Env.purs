module App.Env where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (Reader)
import Data.Either (Either)
import Deku.Core (Nut)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FRP.Event (ZoraEvent, create, fromEvent)
import Paraglider.Operator.ToAff (toAff)
import Platform.Firebase.Auth (User, authStateChangedEventWithAnonymousAccountCreation)
import Platform.Firebase.FbErr (FbErr)
import Platform.Firebase.Firebase (FirebaseEnv, startFirebase)
import Platform.Util.ErrorHandling (liftSuccess)

data AppEvent
  = ShowAppError String

type Env =
  { fb :: FirebaseEnv
  , self :: User

  -- App Bus
  , appEvent :: ZoraEvent AppEvent
  , appPush :: AppEvent -> Effect Unit
  }

type AppNut = Reader Env Nut

type FbEvent a = ZoraEvent (Either FbErr a)

mapFbEvent :: ∀ a b. (a -> b) -> FbEvent a -> FbEvent b
mapFbEvent f ev = map f <$> ev

mkEnv :: Aff (Either FbErr Env)
mkEnv = runExceptT do
  fb <- liftSuccess startFirebase
  self <- liftSuccess $ toAff $ authStateChangedEventWithAnonymousAccountCreation fb.auth
  {push, event} <- liftEffect $ create
  log $ "Loaded env. User: " <> show self
  pure {fb, self, appPush: push, appEvent: fromEvent event }