module Platform.Rx.AffExt where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, joinFiber, killFiber, launchAff, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Halogen.Subscription (Emitter, makeEmitter)

makeEmitterAff :: ∀ a . ((a -> Effect Unit) -> Aff (Effect Unit)) -> Emitter a
makeEmitterAff cb = makeEmitter \k -> do
  fiber <- launchAff $ cb k
  pure $ launchAff_ do
    killFiber (error "") fiber
    eiCleanup <- try $ joinFiber fiber
    case eiCleanup of
      -- We killed the Fiber before it emitted its result
      Left _ -> pure unit
      -- The Fiber completed and we joined with its result
      Right cleanup -> liftEffect cleanup
