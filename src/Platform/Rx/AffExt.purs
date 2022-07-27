module Platform.Rx.AffExt where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, joinFiber, killFiber, launchAff, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Effect.Ref as Ref
import Halogen.Subscription (Emitter, makeEmitter, subscribe, unsubscribe)

rxJust :: ∀ a. a -> Emitter a
rxJust x = makeEmitter \k -> do
  k x
  pure $ pure unit

rxFromCallable :: ∀ a. Effect a -> Emitter a
rxFromCallable effect = makeEmitter \k -> do
  a <- effect
  k a
  pure $ pure unit

rxMakeEmitterAff :: ∀ a . ((a -> Effect Unit) -> Aff (Effect Unit)) -> Emitter a
rxMakeEmitterAff cb = makeEmitter \k -> do
  fiber <- launchAff $ cb k
  pure $ launchAff_ do
    killFiber (error "Event Unsubscribed") fiber
    eiCleanup <- try $ joinFiber fiber
    case eiCleanup of
      -- We killed the Fiber before it emitted its result
      Left _ -> pure unit
      -- The Fiber completed and we joined with its result
      Right cleanup -> liftEffect cleanup

rxFromAff :: Aff ~> Emitter
rxFromAff a = makeEmitter \k -> do
  fib <- launchAff (a >>= liftEffect <<< k)
  pure (launchAff_ (killFiber (error "Event unsubscribed") fib))

-- affMap :: ∀ a b m. MonadAff m => Emitter a -> (a -> m b) -> Emitter b
-- affMap e f =

rxInterval :: Milliseconds -> Emitter Unit
rxInterval x = rxMakeEmitterAff \k -> do
  void $ forever do
    delay x
    liftEffect $ k unit
  pure $ pure unit

rxTake :: ∀ a. Int -> Emitter a -> Emitter a
rxTake n e = makeEmitter \k -> do
  countRef <- Ref.new n
  unsubRef <- Ref.new Nothing
  unsub <- subscribe e \a -> do
    count <- Ref.read countRef
    if count > 0 then do
      Ref.write (count - 1) countRef
      k a
      when (count == 1) do
        mbUnsub <- Ref.read unsubRef
        traverse_ unsubscribe mbUnsub
    else log "take operator had emission with count < 1"

  -- In case the original Emitter emitted at Subscription time
  count <- Ref.read countRef
  if count < 1 then unsubscribe unsub
  else Ref.write (Just unsub) unsubRef
  pure $ unsubscribe unsub

rxCombineLatest :: ∀ a b c. (a -> b -> c) -> Emitter a -> Emitter b -> Emitter c
rxCombineLatest f e1 e2 = makeEmitter \k -> do
    latestA <- Ref.new Nothing
    latestB <- Ref.new Nothing
    c1 <- subscribe e1 \a -> do
      Ref.write (Just a) latestA
      mbB <- Ref.read latestB
      traverse_ (k <<< f a) mbB
    c2 <- subscribe e2 \b -> do
      Ref.write (Just b) latestB
      mbA <- Ref.read latestA
      traverse_ (k <<< (flip f b)) mbA
    pure $ unsubscribe $ c1 <> c2
