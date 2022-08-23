module Platform.Deku.Misc where

import Prelude

import Bolson.Core (envy)
import Bolson.Core as Bolson
import Data.Monoid.Always (class Always)
import Deku.Attribute (Attribute)
import Deku.Control (switcher, text_)
import Deku.Core (class Korok, Domable)
import Deku.DOM (Div_)
import Deku.DOM as D
import Effect (Effect)
import Effect.Class.Console (log)
import FRP.Event (AnEvent, fromEvent, toEvent)
import Paraglider.Operator.DistinctUntilChanged (distinctUntilChangedF)
import Paraglider.Operator.DoOnNext as Paraglider
import Paraglider.Operator.DoOnSubscribe (doOnSubscribe)
import Paraglider.Operator.DoOnUnsubscribe (doOnUnsubscribe)
import Paraglider.Operator.Lift (lift)
import Paraglider.Operator.Replay (replayRefCount)
import Platform.FRP.Wild (WildEvent(..), unwrapWild)
import Platform.FRP.Wild as Wild

type Continuation m logic obj lock a
  = (a -> Bolson.Entity logic obj m lock) -> Bolson.Entity logic obj m lock

wildSwitcher
  :: ∀ a s m error lock payload
   . Korok s m
  => Show error
  => AnEvent m (Attribute Div_)
  -> (Unit -> Domable m lock payload)
  -> WildEvent m error a
  -> Domable m lock payload
wildSwitcher attrs happy wildEv = (distinctUntilChangedF sameWild $ unwrapWild wildEv) #
    switcher D.div attrs case _ of
      Wild.Loading -> text_ "Loading..."
      Wild.Error e -> text_ $ show e
      Wild.Done _ -> happy unit
  where
  sameWild (Wild.Done _) (Wild.Done _) = true
  sameWild (Wild.Loading) (Wild.Loading) = true
  sameWild (Wild.Error _) (Wild.Error _) = true
  sameWild _ _ = false


wrapLogs
  :: forall s m a
   . Korok s m
  => Always (m Unit) (Effect Unit)
  => String
  -> String
  -> AnEvent m a
  -> AnEvent m a
wrapLogs onSub onUnsub e =
  fromEvent $ doOnSubscribe (\_ -> log onSub) $ doOnUnsubscribe (log onUnsub) $ toEvent e

doOnNext
  :: forall s m a
   . Korok s m
  => Always (m Unit) (Effect Unit)
  => (a -> Effect Unit)
  -> AnEvent m a
  -> AnEvent m a
doOnNext onNext e = fromEvent $ (Paraglider.doOnNext onNext $ (toEvent e))

shareEvent
  :: forall s m lock logic obj a
   . Korok s m
  => Always (m Unit) (Effect Unit)
  => AnEvent m a
  -> (AnEvent m a -> Bolson.Entity logic obj m lock)
  -> Bolson.Entity logic obj m lock
shareEvent e f = envy $ lift (f <$> replayRefCount e)

shareWild
  :: forall s m lock logic obj e a
   . Korok s m
  => Always (m Unit) (Effect Unit)
  => WildEvent m e a
  -> (WildEvent m e a -> Bolson.Entity logic obj m lock)
  -> Bolson.Entity logic obj m lock
shareWild (WildEvent e) f = envy $ lift (f <<< WildEvent <$> replayRefCount e)

usingEffect
  :: forall s m lock logic obj a
   . Korok s m
  => Always (m Unit) (Effect Unit)
  => m a
  -> (a -> Bolson.Entity logic obj m lock)
  -> Bolson.Entity logic obj m lock
usingEffect effect f = envy $ lift (f <$> effect)