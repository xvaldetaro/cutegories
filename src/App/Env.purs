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
import FRP.Event (ZoraEvent, Event, create, fromEvent)
import Hyrule.Zora (Zora, liftImpure)
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

  -- Ez helper stuff
  , errPush :: FbErr -> Effect Unit
  , zerrPush :: FbErr -> Zora Unit
  }

type AppNut = Reader Env Nut

type FbEvent a = Event (Either FbErr a)

mapFbEvent :: ∀ a b. (a -> b) -> FbEvent a -> FbEvent b
mapFbEvent f ev = map f <$> ev

mkEnv :: Aff (Either FbErr Env)
mkEnv = runExceptT do
  fb <- liftSuccess startFirebase
  self <- liftSuccess $ toAff $ authStateChangedEventWithAnonymousAccountCreation fb.auth
  {push, event} <- liftEffect $ create
  log $ "Loaded env. User: " <> show self

  let
    errPush e = do
      let s = show e
      log $ "Error pushed" <> s
      push $ ShowAppError s
    zerrPush = errPush >>> liftImpure
  pure {fb, self, errPush, zerrPush, appEvent: fromEvent event }