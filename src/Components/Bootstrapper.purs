module Components.Bootstrapper where

import Prelude

import Components.Router as Router
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Firebase.Firebase (FirebaseEnv, startFirebase)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Monad (runStoreT)
import Store.MyStore as MS
import Type.Proxy (Proxy(..))

type Slots = (router :: H.Slot Router.Query Void Unit)
_router = Proxy :: Proxy "router"


type State m = Maybe
  { routerComponent :: H.Component Router.Query Unit Void m
  , fb :: FirebaseEnv
  }

data Action = Initialize

-- / Shows a loading spinner and Loads all the necessary data for app startup
component :: ∀ q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }
  where
  initialState _ = Nothing

  handleAction :: Action -> H.HalogenM (State m) Action Slots o m Unit
  handleAction = case _ of
    Initialize -> void $ H.fork do
      fb <- H.liftAff startFirebase
      let store = MS.initialStore fb
      routerComponent <- H.liftAff $ runStoreT store MS.reduce Router.component
      H.put $ Just {fb, routerComponent}

  render = case _ of
    Nothing -> HH.text "loading..."
    Just {routerComponent} -> HH.slot_ _router unit routerComponent unit
