module Components.Bootstrapper where

import Prelude

import App.AppM (runAppM)
import Components.Router as Router
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Platform.Firebase.Firebase (FirebaseEnv, startFirebase)
import Type.Proxy (Proxy(..))

type Slots = (router :: H.Slot Router.Query Void Unit)
_router = Proxy :: Proxy "router"

type State = Maybe
  { routerComponent :: H.Component Router.Query Unit Void Aff
  , fb :: FirebaseEnv
  }

data Action = Initialize

-- / Shows a loading spinner and Loads all the necessary data for app startup
component :: ∀ q i o. H.Component q i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }
  where
  initialState _ = Nothing

  handleAction :: Action -> H.HalogenM State Action Slots o Aff Unit
  handleAction = case _ of
    Initialize -> void $ H.fork do
      fb <- H.liftAff startFirebase
      let routerComponent = H.hoist (runAppM {fb}) Router.component
      H.put $ Just {fb, routerComponent}

  render = case _ of
    Nothing -> HH.text "loading..."
    Just {routerComponent} -> HH.slot_ _router unit routerComponent unit
