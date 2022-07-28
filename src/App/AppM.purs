module App.AppM where

import Prelude

import App.Capa.Navigate (class Navigate)
import App.Env (Env)
import App.Route as Route
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Routing.Duplex (print)
import Routing.Hash (setHash)


newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: ∀ a. Env -> AppM a -> Aff a
runAppM env (AppM reader) = runReaderT reader env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadAskAppM :: MonadAsk Env AppM

instance navigateAppM :: Navigate AppM where
  navigate =
    liftEffect <<< setHash <<< print Route.routeCodec
