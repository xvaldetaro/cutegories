module App.Env where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (Reader)
import Data.Either (Either, note)
import Data.Maybe (Maybe)
import Deku.Core (Nut)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FRP.Event (Event, create)
import Paraglider.Operator.ToAff (toAff)
import Platform.Firebase.Auth (User, authStateChangedEventWithAnonymousAccountCreation)
import Platform.Firebase.FbErr (FbErr(..))
import Platform.Firebase.Firebase (FirebaseEnv, startFirebase)
import Platform.Firebase.Synonyms (FbEvent)
import Platform.Util.ErrorHandling (liftSuccess)

data AppEvent
  = ShowAppError String
  | ShowToast String Int

type Env =
  { fb :: FirebaseEnv
  , self :: User

  -- App Bus
  , appEvent :: Event AppEvent

  -- Ez helper stuff
  , appPush :: AppEvent -> Effect Unit
  , errPush :: FbErr -> Effect Unit
  }

type AppNut = Reader Env Nut

forceDocPresent :: ∀ a. Either FbErr (Maybe a) -> Either FbErr a
forceDocPresent  eiMbRoom = eiMbRoom >>= note (DocNotFound "room")

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
  pure {fb, self, errPush, appPush: push, appEvent: event }